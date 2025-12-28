package ndpc.backend

import ndpc.frontend.CheckedProof
import ndpc.frontend.expr.formula._
import scala.annotation.tailrec
import ndpc.frontend.parser._
import cats.syntax.all._
import ndpc.frontend.expr.rule._
import scala.collection.mutable.ReusableBuilder
import scala.annotation.targetName

private type Name = String
private type Pred = (Name, Int) // name + ary
private type Decl = (Set[Pred], Set[Name])

case class LeanExpr(val parts: List[String]) {
    def show: String = parts.mkString(" ")
}
object LeanExpr {
    def ex(g: String, args: Int*) = LeanExpr(g :: args.map(_.toString).toList)

    def ap(es: String*) = LeanExpr(es.map(x => s"h$x").toList)
    def andI(l1: Int, l2: Int) = ex("And.Intro", l1, l2)
    def orL(l: Int) = ex("Or.inl", l)
    def orR(l: Int) = ex("Or.inr", l)
    def ti = LeanExpr(List("True.intro"))
    def iffi(l1: Int, l2: Int) = ex("Iff.intro", l1, l2)
    def exi(name: String, l2: Int) = LeanExpr(List("Exists.intro", name, l2.toString))

    def andL(l: Int) = ex("And.left", l)
    def andR(l: Int) = ex("And.right", l)
    def fe(l: Int) = ex("False.elim", l)
    def mp(l1: Int, l2: String) = LeanExpr(List(s"$l1.mp", l2))
    def em(e: String) = LeanExpr(List("em", e))
    def rfl = ex("rfl")
    def eqsub(l1: Int, l2: Int) = ex("Eq.subst", l1, l2)
    def sym(l: Int) = ex("Eq.symm", l)
}

sealed trait LeanStmt {
    def show(indent: Int): String
}
case class Intro(private val ident: String) extends LeanStmt {
    def this(i: Int) = this(i.toString)

    override def show(indent: Int): String =
        " " * indent + s"intro h$ident"
}
case class Have(val ident: String, val ty: String, val rhs: LeanExpr) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"have h$ident : $ty := ${rhs.show}"
}
case class HaveBy(val l: Int, val ty: String, val rhs: Vector[LeanStmt]) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"have h$l : $ty := by\n" + rhs.map(_.show(indent + 2)).mkString("\n")
}
case class LeanTick(val now: Int, val from: Int) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"have h$now := h$from"
}
case class Exact(val l: Int) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"exact h$l"
}
case class ExactExpr(val e: LeanExpr) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"exact ${e.show}"
}

case object ByContra extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + "apply byContradiction"
}
case object Contra extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + "contradiction"
}
case class Rcases(val from: Int, val c: String, val now: Int) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"rcases h$from with ⟨$c, h$now⟩"
}
case class CaseOr(
    val from: Int,
    val left: Vector[LeanStmt],
    val right: Vector[LeanStmt],
    val rangeL: (Int, Int),
    val rangeR: (Int, Int)
) extends LeanStmt {
    override def show(indent: Int): String =
        val (startL, endL) = rangeL
        val (startR, endR) = rangeR
        val bodyL = left.map(_.show(5)).mkString("\n")
        val bodyR = right.map(_.show(5)).mkString("\n")
        s"""cases h$from with
            | | inl h$startL =>
            |$bodyL
            |     exact h$endL
            | | inr h$startR =>
            |$bodyR
            |     exact h$endR""".stripMargin
            .split("\n")
            .map(" " * indent + _)
            .mkString("\n")
}

extension (c: CheckedProof) {
    def globals: Decl = c.main.flatten
        .map {
            case Empty      => (Set.empty, Set.empty)
            case Comment(_) => (Set.empty, Set.empty)
            case Pf(concl, _, _) =>
                val vars = concl.vars
                (vars.filter(_._2 > 0), vars.filter(_._2 == 0).map(_._1))
        }
        .unzip
        .bimap(_.flatten.toSet, _.flatten.toSet)
}

extension (f: LFormula)
    def asLean: String = f match {
        case PredAp(p, args) =>
            if args == Nil then p
            else s"$p ${args.mkString(" ")}"
        case Eq(left, right) => s"${left.asLean} = ${right.asLean}"
        case Truth           => "True"
        case Falsity         => "False"
        case Not(pf)         => s"¬ (${pf.asLean})"
        case And(left, right) =>
            s"(${left.asLean}) ∧ (${right.asLean})"
        case Or(left, right) =>
            s"(${left.asLean}) ∨ (${right.asLean})"
        case Implies(left, right) =>
            s"(${left.asLean}) → (${right.asLean})"
        case Equiv(left, right) =>
            s"(${left.asLean}) ↔ (${right.asLean})"
        case Forall(x, body) => s"∀ $x : Prop, (${body.asLean})"
        case Exists(x, body) => s"∃ $x : Prop, (${body.asLean})"
    }

object lean extends codegen[Unit] {
    override protected val ext: String = "lean"

    override def compile(pf: CheckedProof, _opt: Unit): String = {
        val pfs = pf.main.flatten.collect { case p: Pf => p }.toVector
        val (premises, body) = pfs.span {
            _.rule match
                case Given | Premise => true
                case _               => false
        }
        given Map[Int, LFormula] = pfs.zipWithIndex.map { (x, i) => (i + 1, x.concl) }.toMap
        build(
          pf.globals,
          premises.map(_.concl.asLean),
          compile(pf.main).map(_.show(2)).mkString("\n"),
          body.last.concl.asLean
        )
    }

    private type State = (
        stash: Vector[LeanStmt],
        lines: ReusableBuilder[LeanStmt, Vector[LeanStmt]],
        stashEnd: Int
    )
    extension (s: State) {
        def clear = (Vector.empty, s.lines, s.stashEnd)
    }

    private def compile(pfs: PfScope, index: Int = 1)(using
        lookup: Map[Int, LFormula]
    ): Vector[LeanStmt] = {
        val (s, ls, _) = pfs.body.zipWithIndex.foldLeft(
          (stash = Vector.empty[LeanStmt], lines = Vector.newBuilder[LeanStmt], stashEnd = 0)
        ) { (acc, x) =>
            x match
                case (Left(Pf(concl, rule, _)), i) =>
                    compilePf(index + i, concl, rule, acc)
                case (Right(scope), i) =>
                    (compile(scope, index + i), acc.lines, index + i + scope.body.length)
                case _ => acc // skip
        }
        ls ++= s
        ls.result()
    }

    private def compilePf(
        now: Int,
        expr: LFormula,
        rule: Rule,
        acc: State
    )(using
        lookup: Map[Int, LFormula]
    ): State = {
        rule match
            // have h : A ∧ B := And.intro h1 h2
            case AndIntro(l, r) =>
                acc.lines += Have(now.toString, expr.asLean, LeanExpr.andI(l, r))
                acc

            // have h5 : A → B := by
            //   intro (h3 : A)
            //   ...
            //   exact h4
            case ImpliesIntro(ass, res) =>
                acc.lines += HaveBy(now, expr.asLean, acc.stash :+ Exact(acc.stashEnd))
                acc.clear

            // have h2 : A ∨ B := Or.inl h1
            // have h3 : B ∨ A := Or.inr h1
            case OrIntro(either) =>
                val Or(left, right) = expr: @unchecked
                val f = if left == lookup(either) then LeanExpr.orL else LeanExpr.orR
                acc.lines += Have(now.toString, expr.asLean, f(either))
                acc

            // have h4 : ¬ A := by
            //   apply byContradiction
            //   intro h2
            //   have h3 : False := ...
            //   contradiction
            case NotIntro(orig, bottom) =>
                acc.lines += HaveBy(now, expr.asLean, ByContra +: acc.stash :+ Contra)
                acc.clear

            // have h4 : ¬ ¬ A := by
            //   intro h
            //   have h2 : False := h h1
            //   exact h2
            case DoubleNegIntro(orig) =>
                acc.lines += HaveBy(
                  now,
                  expr.asLean,
                  Vector(
                    Intro(s"p$now"),
                    Have("p", "False", LeanExpr.ap(s"p$now", orig.toString))
                  )
                )
                acc

            // have h3 : False := h2 h1
            case FalsityIntro(orig, negated) =>
                acc.lines += Have(
                  now.toString,
                  expr.asLean,
                  LeanExpr.ap(negated.toString, orig.toString)
                )
                acc

            // have h : True := True.intro
            case TruthIntro =>
                acc.lines += Have(
                  now.toString,
                  expr.asLean,
                  LeanExpr.ti
                )
                acc

            // have h : A ↔ B := Iff.intro h1 h2
            case EquivIntro(l, r) =>
                acc.lines += Have(now.toString, expr.asLean, LeanExpr.iffi(l, r))
                acc

            // have h : ∃ (x: Prop), P x := Exists.intro A h1
            case ExistsIntro(orig) =>
                val Exists(name, _) = expr: @unchecked
                acc.lines += Have(now.toString, expr.asLean, LeanExpr.exi(name, orig))
                acc

            // have h9 : (∀ (x: Prop), ...) := by
            //   intro x
            //   have h : ... := ...
            //   exact h
            case ForallIntro(const, concl) =>
                acc.lines += HaveBy(now, expr.asLean, acc.stash :+ Exact(acc.stashEnd))
                acc.clear

            // have hA : A := And.left h
            case AndElim(orig) =>
                val And(left, right) = lookup(orig): @unchecked
                val f = if left == expr then LeanExpr.andL else LeanExpr.andR
                acc.lines += Have(now.toString, expr.asLean, f(orig))
                acc

            // have hB : B := h1 h2
            case ImpliesElim(ass, imp) =>
                acc.lines += Have(
                  now.toString,
                  expr.asLean,
                  LeanExpr.ap(imp.toString, ass.toString)
                )
                acc

            // cases h1 with
            // | inl hA =>
            //     have hC : ...
            //     exact hC
            // | inr hB =>
            //     have hC : ...
            //     exact hC
            case OrElim(or, leftAss, leftConcl, rightAss, rightConcl) =>
                val (ls, rs) = acc.stash.splitAt(leftConcl - leftAss)
                acc.lines += CaseOr(
                  or,
                  ls,
                  rs,
                  (leftAss, leftConcl),
                  (rightAss, rightConcl)
                )
                acc.clear

            // have h3 : False := h2 h1
            case NotElim(negated, orig) =>
                acc.lines += Have(
                  now.toString,
                  expr.asLean,
                  LeanExpr.ap(negated.toString, orig.toString)
                )
                acc

            // example {A} (h : ¬¬A) : A := by
            //   apply byContradiction
            //   intro (h1 : ¬A)
            //   have h2 : False := h h1
            //   contradiction
            case DoubleNegElim(orig) =>
                acc.lines += HaveBy(
                  now,
                  expr.asLean,
                  Vector(
                    ByContra,
                    Intro(""),
                    Have("_", "False", LeanExpr(List("h", "h1"))),
                    Contra
                  )
                )
                acc

            // have hA : A := False.elim h
            case FalsityElim(bottom) =>
                acc.lines += Have(now.toString, expr.asLean, LeanExpr.fe(bottom))
                acc

            // example {A B : Prop} (h1 : A ↔ B) (h2 : A) : B := by
            //   have hB : B := h1.mp h2
            //   exact hB
            case EquivElim(equiv, either) =>
                val Implies(left, right) = expr: @unchecked
                acc.lines += HaveBy(
                  now,
                  expr.asLean,
                  Vector(
                    Intro(""),
                    Have("0", left.asLean, LeanExpr.mp(equiv, "")),
                    Exact(0)
                  )
                )
                acc

            // have h : C := by
            //   rcases h1 with ⟨x, hx⟩ -- inserted
            //   have hC : C := h2 x hx
            //   exact hC
            case ExistsElim(exists, ass, concl) =>
                val PredAp(name, Nil) = lookup(ass): @unchecked
                acc.lines += HaveBy(
                  now,
                  expr.asLean,
                  Rcases(exists, name, ass) +: acc.stash :+ Exact(concl)
                )
                acc.clear

            // have hPA : P A := h A
            case ForallElim(orig) =>
                val name = (expr.names -- lookup(orig).names).head
                acc.lines += Have(
                  now.toString,
                  expr.asLean,
                  LeanExpr(List(s"h$orig", name))
                )
                acc

            // example (h1 : ∀ x, P x → Q x) (h2 : ∀ x, P x) : ∀ x, Q x := by
            //   intro x
            //   have hPx : P x := h2 x
            //   have hPQx : P x → Q x := h1 x
            //   have hQx : Q x := hPQx hPx
            //   exact hQx
            case ForallImpElim(ass, imp) =>
                val PredAp(name, Nil) = lookup(ass): @unchecked
                acc.lines += HaveBy(now, expr.asLean, Intro(name) +: acc.stash :+ Exact(imp))
                acc.clear

            // exact em A
            case LEM =>
                val Or(e, _) = expr: @unchecked
                acc.lines += Have(now.toString, expr.asLean, LeanExpr.em(e.asLean))
                acc

            // example {A B : Prop} (h1 : A → B) (h2 : ¬B) : ¬A := by
            //   intro hA
            //   have hB : B := h1 hA
            //   have h_false : False := h2 hB
            //   exact h_false
            case MT(imp, not) =>
                val Not(b) = lookup(not): @unchecked
                acc.lines += HaveBy(
                  now,
                  expr.asLean,
                  Vector(
                    Intro("A"),
                    Have("B", b.asLean, LeanExpr(List(s"h$imp", "hA"))),
                    ExactExpr(LeanExpr(List(s"h$not", "hB")))
                  )
                )
                acc

            // example {A : Prop} (h : ¬ A → False) : A := by
            //   apply byContradiction
            //   intro h1
            //   have h_false : False := h h1
            //   exact h_false
            case PC(orig, bottom) =>
                acc.lines += HaveBy(
                  now,
                  expr.asLean,
                  ByContra
                      +: Intro(s"h$orig") // TODO
                      +: acc.stash
                      :+ Exact(bottom)
                )
                acc.clear

            // have h : A = A := rfl
            case Refl =>
                acc.lines += Have(now.toString, expr.asLean, LeanExpr.rfl)
                acc

            // have h3 : P B := Eq.subst h1 h2
            case EqSub(orig, eq) =>
                acc.lines += Have(now.toString, expr.asLean, LeanExpr.eqsub(eq, orig))
                acc

            // have h1 : B = A := Eq.symm h
            case Sym(orig) =>
                acc.lines += Have(now.toString, expr.asLean, LeanExpr.sym(orig))
                acc
            case ForallIConst =>
                acc.lines += Intro(now.toString)
                acc

            // do nothing, assume place of use will fill it in
            case Ass => acc
            // case Tick(orig)                                           =>
            case Given | Premise => acc
            case _               => ???
    }

    private def ty(arity: Int): String =
        if arity == 1 then "Prop" else s"Prop → ${ty(arity - 1)}"

    private def build(decl: Decl, premises: Vector[String], body: String, result: String) =
        val (preds, vars) = decl
        val predDecls = preds.map((f, n) => s"axiom $f : ${ty(n)}").mkString("\n")
        val varDecls = vars.mkString(" ")
        val premiseDecls = premises.zipWithIndex.map((p, n) => s"  (h$n : $p)").mkString("\n")
        s"""section
            |open Classical
            |$predDecls
            |example {$varDecls : Prop}
            |$premiseDecls
            |: $result := by
            |$body
            |"""
}
