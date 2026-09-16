package ndpc.fuzz

import org.scalacheck.Gen
import ndpc.frontend.parser.{Pf, PfScope, UncheckedProof}
import ndpc.frontend.expr.formula.*
import ndpc.frontend.expr.rule.*
import ndpc.frontend.pretty.*

/** Builds proofs that are valid by construction, mirroring Checker's semantics.
  *
  * Used as a fuzzing oracle: the checker must accept every proof built here, and the codegen
  * backends must not crash on it. If the checker rejects one of these, either the checker has a
  * false negative or this builder is wrong — inspect the counterexample by hand.
  */
object ValidProof:

    case class GeneratedProof(ast: UncheckedProof, text: String)

    // ── document structure ───────────────────────────────────────────────
    sealed trait Node
    case class NLine(nr: Int) extends Node // 1-based line number
    case class NBox(children: List[Node]) extends Node

    case class Scope(
        bodyRev: List[Node],
        known: Vector[(Int, LFormula)] // lines emitted in this scope so far
    )

    case class St(
        all: Vector[Pf], // every proof line, document order (index = nr - 1)
        scopes: List[Scope], // head = current (innermost) scope
        env: Set[String], // names the checker would have accumulated
        premisesPhase: Boolean // root still accepting premise/given
    )

    private def visible(st: St): List[(Int, LFormula)] = st.scopes.flatMap(_.known)

    // ── state transitions ────────────────────────────────────────────────
    private def emit(st: St, concl: LFormula, rule: Rule): (St, Int) =
        val nr = st.all.length + 1
        val pf = Pf(concl, rule, None)
        val sc = st.scopes.head
        val keepPhase = rule match
            case _: Premise.type | _: Given.type => true
            case _                               => false
        val st2 = st.copy(
          all = st.all :+ pf,
          premisesPhase = st.premisesPhase && keepPhase,
          scopes = sc
              .copy(bodyRev = NLine(nr) :: sc.bodyRev, known = sc.known :+ ((nr, concl)))
              :: st.scopes.tail
        )
        (st2, nr)

    private def pushBox(st: St, assConcl: LFormula): (St, Int) =
        val nr = st.all.length + 1
        val pf = Pf(assConcl, Ass, None)
        val fresh = Scope(bodyRev = List(NLine(nr)), known = Vector((nr, assConcl)))
        (
          st.copy(
            all = st.all :+ pf,
            env = st.env ++ assConcl.names,
            scopes = fresh :: st.scopes
          ),
          nr
        )

    private def closeBox(st: St, tickOrig: Int, concl: LFormula): (St, Int) =
        val nr = st.all.length + 1
        val pf = Pf(concl, Tick(tickOrig), None)
        val box = st.scopes.head
        val boxDone =
            box.copy(bodyRev = NLine(nr) :: box.bodyRev, known = box.known :+ ((nr, concl)))
        val parent = st.scopes.tail.head
        (
          st.copy(
            all = st.all :+ pf,
            scopes = parent.copy(bodyRev = NBox(boxDone.bodyRev.reverse) :: parent.bodyRev)
                :: st.scopes.tail.tail
          ),
          nr
        )

    private val nameBases = List("p", "q", "r", "x", "y", "z", "c", "P", "Q", "R")
    private def freshName(used: Set[String]): Gen[String] =
        (for
            b <- Gen.oneOf(nameBases)
            k <- Gen.choose(0, 99)
        yield s"$b$k").retryUntil(n => !used(n))

    // ── rule applications ────────────────────────────────────────────────
    // Simple ops never open a box and do not care about generation depth; box
    // ops do (nesting is capped). Each op returns None when not applicable.
    private type SimpleOp = St => Option[Gen[St]]
    private type BoxOp = (St, Int) => Option[Gen[St]]

    private def premiseLike(rule: Rule): SimpleOp = st =>
        if st.premisesPhase && st.scopes.length == 1 then
            Some(
              FuzzGens.genRoundtripFormula.map { f =>
                  val (s2, _) = emit(st, f, rule)
                  s2.copy(env = s2.env ++ f.names)
              }
            )
        else None

    private def truthOp(st: St): Option[Gen[St]] =
        Some(Gen.const { val (s2, _) = emit(st, Truth, TruthIntro); s2 })

    private def lemOp(st: St): Option[Gen[St]] =
        Some(
          FuzzGens.safeName.map { n =>
              val t = PredAp(n, Nil)
              val (s2, _) = emit(st, Or(t, Not(t)), LEM)
              s2
          }
        )

    private def reflOp(st: St): Option[Gen[St]] =
        if st.env.isEmpty then None
        else
            Some(
              Gen.oneOf(st.env.toList).map { n =>
                  val t = PredAp(n, Nil)
                  val (s2, _) = emit(st, Eq(t, t), Refl)
                  s2
              }
            )

    private def forallIConstOp(st: St): Option[Gen[St]] =
        Some(
          freshName(st.env).map { c =>
              val (s2, _) = emit(st, PredAp(c, Nil), ForallIConst)
              s2.copy(env = s2.env + c)
          }
        )

    private def andIntroOp(st: St): Option[Gen[St]] =
        val v = visible(st)
        Some(
          for
              (n1, f1) <- Gen.oneOf(v)
              (n2, f2) <- Gen.oneOf(v)
          yield { val (s2, _) = emit(st, And(f1, f2), AndIntro(n1, n2)); s2 }
        )

    private def orIntroOp(st: St): Option[Gen[St]] =
        val v = visible(st)
        Some(
          for
              (n, f) <- Gen.oneOf(v)
              fresh <- FuzzGens.safeName
              left <- Gen.oneOf(true, false)
          yield
              val concl = if left then Or(f, PredAp(fresh, Nil)) else Or(PredAp(fresh, Nil), f)
              val (s2, _) = emit(st, concl, OrIntro(n))
              s2
        )

    private def dnegIntroOp(st: St): Option[Gen[St]] =
        val v = visible(st)
        Some(
          Gen.oneOf(v).map { case (n, f) =>
              val (s2, _) = emit(st, Not(Not(f)), DoubleNegIntro(n))
              s2
          }
        )

    private def dnegElimOp(st: St): Option[Gen[St]] =
        val v = visible(st).collect { case (n, Not(Not(x))) => (n, x) }
        if v.isEmpty then None
        else
            Some(
              Gen.oneOf(v).map { case (n, x) =>
                  val (s2, _) = emit(st, x, DoubleNegElim(n))
                  s2
              }
            )

    private def andElimOp(st: St): Option[Gen[St]] =
        val v = visible(st).collect { case (n, And(l, r)) => (n, l, r) }
        if v.isEmpty then None
        else
            Some(
              for
                  (n, l, r) <- Gen.oneOf(v)
                  takeLeft <- Gen.oneOf(true, false)
              yield
                  val (s2, _) = emit(st, if takeLeft then l else r, AndElim(n))
                  s2
            )

    private def impliesElimOp(st: St): Option[Gen[St]] =
        val v = visible(st)
        val usable = v.flatMap { case (inr, imp) =>
            imp match
                case Implies(a, b) =>
                    v.collect { case (anr, af) if af == a => (inr, anr, b) }
                case _ => Nil
        }
        if usable.isEmpty then None
        else
            Some(
              Gen.oneOf(usable).map { case (inr, anr, b) =>
                  // NOTE: Rule.ImpliesElim names its fields (ass, imp), but the user
                  // syntax and the checker both treat the FIRST number as the
                  // implication line (see fol_l.ndp `->E(4,3)`).
                  val (s2, _) = emit(st, b, ImpliesElim(inr, anr))
                  s2
              }
            )

    private def equivElimOp(st: St): Option[Gen[St]] =
        val v = visible(st)
        val usable = v.flatMap { case (enr, eq) =>
            eq match
                case Equiv(a, b) =>
                    v.collect {
                        case (xnr, xf) if xf == a => (enr, xnr, b)
                        case (xnr, xf) if xf == b => (enr, xnr, a)
                    }
                case _ => Nil
        }
        if usable.isEmpty then None
        else
            Some(
              Gen.oneOf(usable).map { case (enr, xnr, res) =>
                  val (s2, _) = emit(st, res, EquivElim(enr, xnr))
                  s2
              }
            )

    private def notElimOp(st: St): Option[Gen[St]] =
        val v = visible(st)
        val usable = v.flatMap { case (nnr, nf) =>
            nf match
                case Not(x) => v.collect { case (onr, of) if of == x => (nnr, onr) }
                case _      => Nil
        }
        if usable.isEmpty then None
        else
            Some(
              Gen.oneOf(usable).map { case (nnr, onr) =>
                  val (s2, _) = emit(st, Falsity, NotElim(nnr, onr))
                  s2
              }
            )

    private def falsityIntroOp(st: St): Option[Gen[St]] =
        val v = visible(st)
        val usable = v.flatMap { case (onr, of) =>
            v.collect { case (nnr, Not(x)) if x == of => (onr, nnr) }
        }
        if usable.isEmpty then None
        else
            Some(
              Gen.oneOf(usable).map { case (onr, nnr) =>
                  val (s2, _) = emit(st, Falsity, FalsityIntro(onr, nnr))
                  s2
              }
            )

    private def falsityElimOp(st: St): Option[Gen[St]] =
        val v = visible(st).collect { case it @ (_, Falsity) => it }
        if v.isEmpty then None
        else
            Some(
              for
                  (bnr, _) <- Gen.oneOf(v)
                  f <- FuzzGens.genRoundtripFormula
              yield
                  val (s2, _) = emit(st, f, FalsityElim(bnr))
                  s2.copy(env = s2.env ++ f.names)
            )

    private def equivIntroOp(st: St): Option[Gen[St]] =
        val v = visible(st)
        val usable = for
            case (n1, Implies(a, b)) <- v
            case (n2, Implies(c, d)) <- v
            if a == d && b == c
        yield (n1, n2, a, b)
        if usable.isEmpty then None
        else
            Some(
              Gen.oneOf(usable).map { case (n1, n2, a, b) =>
                  val (s2, _) = emit(st, Equiv(a, b), EquivIntro(n1, n2))
                  s2
              }
            )

    private def mtOp(st: St): Option[Gen[St]] =
        val v = visible(st)
        val usable = for
            case (inr, Implies(a, b)) <- v
            case (nnr, Not(x)) <- v
            if x == b
        yield (inr, nnr, a)
        if usable.isEmpty then None
        else
            Some(
              Gen.oneOf(usable).map { case (inr, nnr, a) =>
                  val (s2, _) = emit(st, Not(a), MT(inr, nnr))
                  s2
              }
            )

    private def symOp(st: St): Option[Gen[St]] =
        val v = visible(st).collect { case it @ (_, Eq(l, r)) => (it, l, r) }
        if v.isEmpty then None
        else
            Some(
              Gen.oneOf(v).map { case ((n, _), l, r) =>
                  val (s2, _) = emit(st, Eq(r, l), Sym(n))
                  s2
              }
            )

    private def eqSubOp(st: St): Option[Gen[St]] =
        val v = visible(st)
        val eqs = v.collect { case it @ (_, _: Eq) => it }
        val origs = v.collect { case it @ (_, f) if !f.isInstanceOf[Eq] => it }
        if eqs.isEmpty || origs.isEmpty then None
        else
            Some(
              for
                  case (enr, Eq(a, b)) <- Gen.oneOf(eqs)
                  (onr, of) <- Gen.oneOf(origs)
                  concl <- Gen.oneOf(of.substitutes(a, b).toList)
              yield { val (s2, _) = emit(st, concl, EqSub(onr, enr)); s2 }
            )

    private def existsIntroOp(st: St): Option[Gen[St]] =
        val v = visible(st)
        Some(
          for
              (n, f) <- Gen.oneOf(v)
              x <- freshName(f.names)
          yield { val (s2, _) = emit(st, Exists(x, f), ExistsIntro(n)); s2 }
        )

    private def forallElimOp(st: St): Option[Gen[St]] =
        val v = visible(st).collect { case it @ (_, Forall(_, b)) => (it, b) }
        if v.isEmpty then None
        else
            Some(
              Gen.oneOf(v).map { case ((n, _), body) =>
                  val (s2, _) = emit(st, body, ForallElim(n))
                  s2
              }
            )

    private def forallImpElimOp(st: St): Option[Gen[St]] =
        val v = visible(st)
        val usable = v.flatMap { case (inr, imp) =>
            imp match
                case Forall(_, Implies(a, b)) =>
                    v.collect { case (anr, af) if af == a => (inr, anr, b) }
                case _ => Nil
        }
        if usable.isEmpty then None
        else
            Some(
              Gen.oneOf(usable).map { case (inr, anr, b) =>
                  val (s2, _) = emit(st, b, ForallImpElim(anr, inr))
                  s2
              }
            )

    // ── box-opening rules ────────────────────────────────────────────────
    private def impliesIntroOp(st: St, depth: Int): Option[Gen[St]] =
        Some(
          FuzzGens.genRoundtripFormula.flatMap { a =>
              val (st1, assNr) = pushBox(st, a)
              innerSteps(st1, depth).flatMap { st2 =>
                  val inner = st2.scopes.head.known
                  Gen.oneOf(inner.toList).flatMap { case (cnr, cf) =>
                      val (st3, tickNr) = closeBox(st2, cnr, cf)
                      val (st4, _) = emit(st3, Implies(a, cf), ImpliesIntro(assNr, tickNr))
                      Gen.const(st4)
                  }
              }
          }
        )

    private def notIntroOp(st: St, depth: Int): Option[Gen[St]] =
        // NotIntro: assume φ in a box, derive ⊥ (using a visible ¬φ), conclude ¬φ
        val negs = visible(st).collect { case (nnr, Not(x)) => (nnr, x) }
        if negs.isEmpty then None
        else
            Some(
              Gen.oneOf(negs).flatMap { case (nnr, x) =>
                  val (st1, assNr) = pushBox(st, x)
                  val (st2, fnr) = emit(st1, Falsity, NotElim(nnr, assNr))
                  val (st3, tickNr) = closeBox(st2, fnr, Falsity)
                  val (st4, _) = emit(st3, Not(x), NotIntro(assNr, tickNr))
                  Gen.const(st4)
              }
            )

    private def pcOp(st: St, depth: Int): Option[Gen[St]] =
        val v = visible(st)
        if v.isEmpty then None
        else
            Some(
              Gen.oneOf(v).flatMap { case (fnr, f) =>
                  val (st1, assNr) = pushBox(st, Not(f))
                  val (st2, bnr) = emit(st1, Falsity, NotElim(assNr, fnr))
                  val (st3, tickNr) = closeBox(st2, bnr, Falsity)
                  val (st4, _) = emit(st3, f, PC(assNr, tickNr))
                  Gen.const(st4)
              }
            )

    private def forallIntroOp(st: St, depth: Int): Option[Gen[St]] =
        Some(
          freshName(st.env).flatMap { c =>
              val (st1, assNr) = pushBox(st, PredAp(c, Nil))
              val maybeInner =
                  if depth + 1 < 2 then
                      Gen.oneOf(true, false).flatMap {
                          case true  => step(st1, depth + 1)
                          case false => Gen.const(st1)
                      }
                  else step(st1, depth + 1)
              maybeInner.flatMap { st2 =>
                  val (cnr, cf) = st2.scopes.head.known.last
                  freshName(cf.names).flatMap { x =>
                      val (st3, tickNr) = closeBox(st2, cnr, cf)
                      val (st4, _) = emit(st3, Forall(x, cf), ForallIntro(assNr, tickNr))
                      Gen.const(st4)
                  }
              }
          }
        )

    private def existsElimOp(st: St, depth: Int): Option[Gen[St]] =
        val cands = visible(st).collect {
            case (nr, Exists(x, body)) if !body.names(x) =>
                (nr, body)
        }
        if cands.isEmpty then None
        else
            Some(
              Gen.oneOf(cands).flatMap { case (enr, body) =>
                  val (st1, assNr) = pushBox(st, body)
                  val (st2, tnr) = emit(st1, Truth, TruthIntro)
                  val (st3, tickNr) = closeBox(st2, tnr, Truth)
                  val (st4, _) = emit(st3, Truth, ExistsElim(enr, assNr, tickNr))
                  Gen.const(st4)
              }
            )

    private def orElimOp(st: St, depth: Int): Option[Gen[St]] =
        val cands = visible(st).collect { case (nr, Or(a, b)) => (nr, a, b) }
        if cands.isEmpty then None
        else
            Some(
              Gen.oneOf(cands).flatMap { case (onr, a, b) =>
                  val (st1, a1) = pushBox(st, a)
                  val (st2, t1) = emit(st1, Truth, TruthIntro)
                  val (st3, c1) = closeBox(st2, t1, Truth)
                  val (st4, a2) = pushBox(st3, b)
                  val (st5, t2) = emit(st4, Truth, TruthIntro)
                  val (st6, c2) = closeBox(st5, t2, Truth)
                  val (st7, _) = emit(st6, Truth, OrElim(onr, a1, c1, a2, c2))
                  Gen.const(st7)
              }
            )

    // ── the op pool ──────────────────────────────────────────────────────
    private def simpleOps: List[(Int, SimpleOp)] = List(
      (2, premiseLike(Premise)),
      (2, premiseLike(Given)),
      (2, truthOp),
      (1, lemOp),
      (1, reflOp),
      (1, forallIConstOp),
      (2, andIntroOp),
      (2, orIntroOp),
      (2, dnegIntroOp),
      (2, dnegElimOp),
      (2, andElimOp),
      (2, impliesElimOp),
      (2, equivElimOp),
      (2, notElimOp),
      (2, falsityIntroOp),
      (2, falsityElimOp),
      (1, equivIntroOp),
      (2, mtOp),
      (1, symOp),
      (1, eqSubOp),
      (2, existsIntroOp),
      (2, forallElimOp),
      (2, forallImpElimOp)
    )

    private def boxOps: List[(Int, BoxOp)] = List(
      (2, impliesIntroOp),
      (1, notIntroOp),
      (1, pcOp),
      (1, forallIntroOp),
      (1, existsElimOp),
      (1, orElimOp)
    )

    /** Box nesting is capped at this generation depth. */
    private def boxable(depth: Int): Boolean = depth < 2

    private def step(st: St, depth: Int): Gen[St] =
        val candidates = simpleOps.flatMap((w, op) => op(st).map(g => (w, g))) ++
            (if boxable(depth) then boxOps.flatMap((w, op) => op(st, depth).map(g => (w, g)))
             else Nil)
        if candidates.isEmpty then Gen.const(st) // cannot happen: truthOp always applies
        else Gen.frequency(candidates*)

    private def runSteps(n: Int, depth: Int, st: St): Gen[St] =
        if n <= 0 then Gen.const(st)
        else step(st, depth).flatMap(runSteps(n - 1, depth, _))

    private def innerSteps(st: St, depth: Int): Gen[St] =
        for
            n <- Gen.choose(0, 2)
            s <- runSteps(n, depth + 1, st)
        yield s

    // ── assembly ─────────────────────────────────────────────────────────
    private def mkScope(nodes: List[Node], all: Vector[Pf]): PfScope =
        PfScope(nodes.map {
            case NLine(nr)      => Left(all(nr - 1))
            case NBox(children) => Right(mkScope(children, all))
        })

    private def printNodes(nodes: List[Node], all: Vector[Pf], indent: Int): String =
        nodes.map {
            case NLine(nr) =>
                val Pf(concl, rule, _) = all(nr - 1)
                s"${"  " * indent}${concl.pretty} [${rule.pretty}]\n"
            case NBox(children) => printNodes(children, all, indent + 1)
        }.mkString

    val gen: Gen[GeneratedProof] =
        for
            nPre <- Gen.choose(1, 3)
            prems <- Gen.listOfN(nPre, FuzzGens.genRoundtripFormula)
            st0 = prems.foldLeft(
              St(Vector.empty, List(Scope(Nil, Vector.empty)), Set.empty, true)
            ) { (s, f) =>
                val (s2, _) = emit(s, f, Premise)
                s2.copy(env = s2.env ++ f.names)
            }
            nSteps <- Gen.choose(2, 10)
            st1 <- runSteps(nSteps, 0, st0)
        yield
            val root = st1.scopes.head
            val nodes = root.bodyRev.reverse
            val ast = UncheckedProof(mkScope(nodes, st1.all), st1.all.toList)
            GeneratedProof(ast, printNodes(nodes, st1.all, 0))
