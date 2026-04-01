// Typing in this file is terrible, consider refactoring
package ndpc.cli.backend

import cats.effect.IO
import ndpc.frontend.CheckedProof
import ndpc.cli.CliRuntime
import ndpc.frontend.expr.formula.*
import scala.annotation.tailrec
import ndpc.frontend.parser.*
import cats.syntax.all.*
import ndpc.frontend.pretty
import ndpc.frontend.expr.rule.*
import scala.collection.mutable.ReusableBuilder
import scala.annotation.targetName

private def h(x: Any) = s"h$x"
private def tmp(x: Any) = s"tmp$x"
case class LeanExpr(val parts: List[String]) {
    def show: String = parts.mkString(" ")
}
object LeanExpr {
    def ex(g: String, args: Int*) = LeanExpr(g :: args.map(h).toList)

    def ap(es: Int*) = LeanExpr(es.map(h).toList)
    def andI(l1: Int, l2: Int) = ex("And.intro", l1, l2)
    def orL(l: Int) = ex("Or.inl", l)
    def orR(l: Int) = ex("Or.inr", l)
    def ti = LeanExpr(List("True.intro"))
    def iffi(l1: Int, l2: Int) = ex("Iff.intro", l1, l2)
    def exi(name: String, l2: Int) = LeanExpr(List("Exists.intro", name, s"h$l2"))

    def andL(l: Int) = ex("And.left", l)
    def andR(l: Int) = ex("And.right", l)
    def fe(l: Int) = ex("False.elim", l)
    def mp(l1: Int, l2: Int, leftRight: Boolean) = LeanExpr(
      List(if leftRight then s"h$l1.mp" else s"h$l1.mpr", h(l2))
    )
    def em(e: String) = LeanExpr(List("em", e))
    def rfl = ex("rfl")
    def sym(l: Int) = ex("Eq.symm", l)
}

sealed trait LeanStmt {
    def show(indent: Int): String
}
object LeanStmt {
    // lean stmt -> corresponding #lines in ndp
    def length(s: LeanStmt): Int = s match
        case it: HaveBy => it.lines
        case CaseOr(from, left, right, rangeL, rangeR) =>
            1 + (rangeL._2 - rangeL._1) + (rangeR._2 - rangeR._1)
        case _ => 1
}
case class Intro(val ident: String) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"intro $ident"
}
object Intro {
    def line(i: Int) = Intro(h(i))
}
case class Have(val ident: String, val ty: LFormula, val rhs: LeanExpr) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"have h$ident : ${ty.asLean} := ${rhs.show}"
}
case class HaveBy(val l: Int, val ty: LFormula, val rhs: Vector[LeanStmt], val lines: Int)
    extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"have h$l : ${ty.asLean} := by\n" + rhs
            .map(_.show(indent + 2))
            .mkString("\n")
}
object HaveBy {
    // private def make(f: Int => Int): (Int, LFormula, Vector[LeanStmt]) => HaveBy =
    //     (l, ty, rhs) => HaveBy(l, ty, rhs, f(rhs.map(LeanStmt.length).sum))
    private def make(f: Int => Int): (Int, LFormula, Vector[LeanStmt]) => HaveBy =
        (l, ty, rhs) => {
            // println("===============")
            // println(rhs.map(_.show(0)).mkString("\n"))
            // println(s"length: ${f(rhs.map(LeanStmt.length).sum)}")
            // println("===============")
            HaveBy(l, ty, rhs, f(rhs.map(LeanStmt.length).sum))
        }

    def impi = make(_ - 1)
    def noti = make(_ - 1)
    def dni = make(_ => 1)
    def fai = make(identity)
    def ore = make(identity)
    def dne = make(_ => 1)
    def exe = make(identity)
    def mt = make(_ => 1)
    def pc = make(_ - 1)
    def eqsub = make(_ => 1)
}
case class Rw(val l: Int, val symm: Boolean) extends LeanStmt {
    override def show(indent: Int): String =
        val rule = if symm then s"← h$l" else h(l)
        " " * indent + s"rw [$rule]"
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
            ~| inl h$startL =>
            ~$bodyL
            ~     exact h$endL
            ~| inr h$startR =>
            ~${bodyR}
            ~     exact h$endR"""
            .stripMargin('~')
            .split("\n")
            .map(" " * indent + _)
            .mkString("\n")
}

extension (c: CheckedProof) {
    def globals: Set[Symbol] = c.main.flatten.flatMap {
        case Empty                       => Set.empty
        case Comment(_)                  => Set.empty
        case Pf(PredAp(name, Nil), _, _) => Set(Predicate(name, 0))
        case Pf(concl, _, _)             => concl.symbols
    }.toSet
}

extension (f: LFormula) {
    def asLean: String = f match {
        case PredAp(p, args) =>
            if args == Nil then p
            else s"($p ${args.map(_.asLean).mkString(" ")})"
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
        case Forall(x, body) => s"∀ $x : U, (${body.asLean})"
        case Exists(x, body) => s"∃ $x : U, (${body.asLean})"
    }

    def diff(other: LFormula): Option[LFormula] = {
        (f, other) match
            case (PredAp(x, Nil), it @ PredAp(y, Nil)) if x != y => Some(it)
            case (Eq(l1, r1), Eq(l2, r2)) =>
                l1.diff(l2) orElse r1.diff(r2)
            case (And(l1, r1), And(l2, r2)) =>
                l1.diff(l2) orElse r1.diff(r2)
            case (Or(l1, r1), Or(l2, r2)) =>
                l1.diff(l2) orElse r1.diff(r2)
            case (Implies(l1, r1), Implies(l2, r2)) =>
                l1.diff(l2) orElse r1.diff(r2)
            case (Equiv(l1, r1), Equiv(l2, r2)) =>
                l1.diff(l2) orElse r1.diff(r2)
            case (Not(p1), Not(p2)) => p1.diff(p2)
            case (Forall(_, body1), Forall(_, body2)) =>
                body1.diff(body2)
            case (Exists(_, body1), Exists(_, body2)) =>
                body1.diff(body2)
            case (PredAp(p1, args1), r @ PredAp(p2, args2)) =>
                if p1 == p2 && args1.length == args2.length then
                    args1.zip(args2).foldLeft(None) {
                        case (None, (a1, a2))   => a1.diff(a2)
                        case (acc @ Some(_), _) => acc
                    }
                else Some(r)
            case _ => None
    }
}

object lean extends Codegen[Unit] {
    override protected val ext: String = "lean"

    override def compile(pf: CheckedProof, _opt: Unit, _runtime: CliRuntime): IO[String] = IO.pure {
        val pfs = pf.main.flatten.collect { case p: Pf => p }.toVector
        val (premises, body) = pfs.span {
            _.rule match
                case Given | Premise => true
                case _               => false
        }
        given Map[Int, LFormula] = pfs.zipWithIndex.map { (x, i) => (i + 1, x.concl) }.toMap
        val proof = compile(pf.main, 1) :+ Exact(pfs.length)

        build(
          pf.globals,
          premises.map(_.concl.asLean),
          proof.map(_.show(2)).mkString("\n"),
          body.last.concl.asLean
        )
    }

    case class State(
        stash: Vector[LeanStmt],
        lines: ReusableBuilder[LeanStmt, Vector[LeanStmt]],
        stashEnd: Int,
        linenr: Int
    )
    extension (s: State) {
        def clear = State(Vector.empty, s.lines, s.stashEnd, s.linenr)
        def incr = State(s.stash, s.lines, s.stashEnd, s.linenr + 1)
    }

    private def compile(pfs: PfScope, index: Int)(using
        lookup: Map[Int, LFormula]
    ): Vector[LeanStmt] = {
        val acc = pfs.body.foldLeft(
          State(
            stash = Vector.empty[LeanStmt],
            lines = Vector.newBuilder[LeanStmt],
            stashEnd = 0,
            linenr = index
          )
        ) { (acc, x) =>
            x match
                case Left(Pf(concl, rule, _)) =>
                    compilePf(acc.linenr, concl, rule, acc).incr
                case Right(scope) =>
                    val n = scope.flatten.count {
                        case _: Pf => true
                        case _     => false
                    }
                    State(
                      acc.stash ++ compile(scope, acc.linenr),
                      acc.lines,
                      acc.linenr + n - 1,
                      acc.linenr + n
                    )
                case _ => acc // skip
        }
        acc.lines ++= acc.stash
        acc.lines.result()
    }

    private def compilePf(
        now: Int,
        expr: LFormula,
        rule: Rule,
        acc: State
    )(using
        lookup: Map[Int, LFormula]
    ): State = {
        // println(s"$now: ${expr.pretty} ${rule}")
        rule match
            // have h : A ∧ B := And.intro h1 h2
            case AndIntro(l, r) =>
                acc.lines += Have(now.toString, expr, LeanExpr.andI(l, r))
                acc

            // have h5 : A → B := by
            //   intro (h3 : A)
            //   ...
            //   exact h4
            case ImpliesIntro(ass, res) =>
                acc.lines += HaveBy.impi(
                  now,
                  expr,
                  Intro(h(ass)) +: acc.stash :+ Exact(acc.stashEnd)
                )
                acc.clear

            // have h2 : A ∨ B := Or.inl h1
            // have h3 : B ∨ A := Or.inr h1
            case OrIntro(either) =>
                val Or(left, right) = expr: @unchecked
                val f = if left == lookup(either) then LeanExpr.orL else LeanExpr.orR
                acc.lines += Have(now.toString, expr, f(either))
                acc

            // have h4 : ¬ A := by
            //   intro h2
            //   have h3 : False := ...
            //   exact h3
            case NotIntro(orig, bottom) =>
                acc.lines += HaveBy.noti(
                  now,
                  expr,
                  Intro.line(orig) +: acc.stash :+ Exact(bottom)
                )
                acc.clear

            // have h4 : ¬ ¬ A := by
            //   intro h
            //   have h2 : False := h h1
            //   exact h2
            case DoubleNegIntro(orig) =>
                acc.lines += HaveBy.dni(
                  now,
                  expr,
                  Vector(
                    Intro(tmp(now)),
                    ExactExpr(LeanExpr(List(tmp(now), h(orig))))
                  )
                )
                acc

            // have h3 : False := h2 h1
            case FalsityIntro(orig, negated) =>
                acc.lines += Have(
                  now.toString,
                  expr,
                  LeanExpr.ap(negated, orig)
                )
                acc

            // have h : True := True.intro
            case TruthIntro =>
                acc.lines += Have(
                  now.toString,
                  expr,
                  LeanExpr.ti
                )
                acc

            // have h : A ↔ B := Iff.intro h1 h2
            case EquivIntro(l, r) =>
                acc.lines += Have(now.toString, expr, LeanExpr.iffi(l, r))
                acc

            // have h : ∃ (x: Prop), P x := Exists.intro A h1
            case ExistsIntro(orig) =>
                val Exists(_, ex) = expr: @unchecked
                val Some(PredAp(name, Nil)) = ex.diff(lookup(orig)): @unchecked
                acc.lines += Have(now.toString, expr, LeanExpr.exi(name, orig))
                acc

            // have h9 : (∀ (x: Prop), ...) := by
            //   intro x
            //   have h : ... := ...
            //   exact h
            case ForallIntro(const, concl) =>
                acc.lines += HaveBy.fai(now, expr, acc.stash :+ Exact(acc.stashEnd))
                acc.clear

            // have hA : A := And.left h
            case AndElim(orig) =>
                val And(left, right) = lookup(orig): @unchecked
                val f = if left == expr then LeanExpr.andL else LeanExpr.andR
                acc.lines += Have(now.toString, expr, f(orig))
                acc

            // have hB : B := h1 h2
            case ImpliesElim(ass, imp) =>
                acc.lines += Have(
                  now.toString,
                  expr,
                  LeanExpr.ap(ass, imp)
                )
                acc

            // cases h1 with
            // | inl hA =>
            //     have hC : ...
            //     exact hC
            // | inr hB =>
            //     have hC : ...
            //     exact hC
            case it @ OrElim(or, leftAss, leftConcl, rightAss, rightConcl) =>
                // println(it)
                // println("stash:")
                // println(acc.stash.map(_.show(0)).mkString("\n"))
                val (ls, rs) = split(acc.stash, leftConcl - leftAss)
                // println("--------------------------")
                // println(ls.map(_.show(0)).mkString("\n"))
                // println("--------------------------")
                // println(rs.map(_.show(0)).mkString("\n"))
                // println("--------------------------")
                acc.lines += HaveBy.ore(
                  now,
                  expr,
                  Vector(
                    CaseOr(
                      or,
                      ls,
                      rs,
                      (leftAss, leftConcl),
                      (rightAss, rightConcl)
                    )
                  )
                )
                acc.clear

            // have h3 : False := h2 h1
            case NotElim(negated, orig) =>
                acc.lines += Have(
                  now.toString,
                  expr,
                  LeanExpr.ap(negated, orig)
                )
                acc

            // example {A} (h : ¬¬A) : A := by
            //   apply byContradiction
            //   intro (h1 : ¬A)
            //   have h2 : False := h h1
            //   contradiction
            case DoubleNegElim(orig) =>
                acc.lines += HaveBy.dne(
                  now,
                  expr,
                  Vector(
                    ByContra,
                    Intro("tmp"),
                    Have(tmp(now), Falsity, LeanExpr(List(h(orig), "tmp"))),
                    Contra
                  )
                )
                acc

            // have hA : A := False.elim h
            case FalsityElim(bottom) =>
                acc.lines += Have(now.toString, expr, LeanExpr.fe(bottom))
                acc

            // example {A B : Prop} (h1 : A ↔ B) (h2 : A) : B := by
            //   have hB : B := h1.mp h2
            //   exact hB
            case EquivElim(equiv, either) =>
                val Equiv(l, r) = lookup(equiv): @unchecked
                val lr = lookup(either) == l
                acc.lines += Have(
                  now.toString,
                  expr,
                  LeanExpr.mp(equiv, either, lr)
                )
                acc

            // have h : C := by
            //   rcases h1 with ⟨x, hx⟩ -- inserted
            //   have hC : C := h2 x hx
            //   exact hC
            case ExistsElim(exists, ass, concl) =>
                val Exists(_, ex) = lookup(exists): @unchecked
                val Some(PredAp(name, Nil)) = ex.diff(lookup(ass)): @unchecked
                acc.lines += HaveBy.exe(
                  now,
                  expr,
                  Rcases(exists, name, ass) +: acc.stash :+ Exact(concl)
                )
                acc.clear

            // have hPA : P A := h A
            case ForallElim(orig) =>
                val Forall(_, fa) = lookup(orig): @unchecked
                val Some(substituted) = fa.diff(expr): @unchecked
                acc.lines += Have(
                  now.toString,
                  expr,
                  LeanExpr(List(h(orig), substituted.asLean))
                )
                acc

            // have h9 : (g b) = (g a) := Eq.subst (Eq.symm h2) h8
            // have h10 : ∀ y : Prop, (((g b) = (g y)) → (b = y)) := h3 b
            // have h11 : b = a := h10 a h9
            case ForallImpElim(ass, imp) =>
                val Forall(x, impBody) = lookup(imp): @unchecked
                val name = (expr.names -- (impBody.names - x)).head
                acc.lines += Have(now.toString, expr, LeanExpr(List(h(imp), name, h(ass))))
                acc

            // exact em A
            case LEM =>
                val Or(e, _) = expr: @unchecked
                acc.lines += Have(now.toString, expr, LeanExpr.em(e.asLean))
                acc

            // example {A B : Prop} (h1 : A → B) (h2 : ¬B) : ¬A := by
            //   intro hA
            //   have hB : B := h1 hA
            //   have h_false : False := h2 hB
            //   exact h_false
            case MT(imp, not) =>
                val Not(b) = lookup(not): @unchecked
                acc.lines += HaveBy.mt(
                  now,
                  expr,
                  Vector(
                    Intro("tmp"),
                    Have(tmp(now), b, LeanExpr(List(h(imp), "tmp"))),
                    ExactExpr(LeanExpr(List(h(not), h(tmp(now)))))
                  )
                )
                acc

            // example {A : Prop} (h : ¬ A → False) : A := by
            //   apply byContradiction
            //   intro h1
            //   have h_false : False := h h1
            //   exact h_false
            case PC(orig, bottom) =>
                acc.lines += HaveBy.pc(
                  now,
                  expr,
                  ByContra
                      +: Intro.line(orig) // TODO
                      +: acc.stash
                      :+ Exact(bottom)
                )
                acc.clear

            // have h : A = A := rfl
            case Refl =>
                acc.lines += Have(now.toString, expr, LeanExpr.rfl)
                acc

            // have h3 : P B := Eq.subst h1 h2
            case EqSub(orig, eq) =>
                val Eq(_, r) = lookup(eq): @unchecked
                val symm = lookup(orig).diff(expr).get == r
                acc.lines += HaveBy.eqsub(
                  now,
                  expr,
                  Vector(
                    Rw(eq, symm),
                    Exact(orig)
                  )
                )
                acc

            // have h1 : B = A := Eq.symm h
            case Sym(orig) =>
                acc.lines += Have(now.toString, expr, LeanExpr.sym(orig))
                acc
            case ForallIConst =>
                acc.lines += Intro(expr.asLean)
                acc

            // do nothing, assume place of use will fill it in
            case Ass => acc
            case Tick(orig) =>
                acc.lines += Have(now.toString, expr, LeanExpr(List(h(orig))))
                acc
            case Given | Premise => acc
    }

    private def predty(arity: Int): String =
        if arity == 0 then "Prop" else s"U → ${predty(arity - 1)}"
    private def functy(arity: Int): String =
        if arity == 0 then "U" else s"U → ${functy(arity - 1)}"

    private def build(decl: Set[Symbol], premises: Vector[String], body: String, result: String) =
        val preds = decl.collect { case Predicate(name, arity) => (name, arity) }
        val vars = decl.collect { case Var(name) => name }
        val funcs = decl.collect { case Function(name, arity) => (name, arity) }
        // println(decl)

        val predDecls = preds.map((f, n) => s"variable ($f : ${predty(n)})").mkString("\n")
        val varDecls = if vars.isEmpty then "" else s"{${vars.mkString(" ")}: U}"
        val funcDecls = funcs.map((f, n) => s"variable ($f : ${functy(n)})").mkString("\n")

        val premiseDecls = premises.zipWithIndex.map((p, n) => s"  (h${n + 1} : $p)").mkString("\n")
        s"""-- `lean *.lean` or https://live.lean-lang.org/
            ~section
            ~open Classical
            ~set_option linter.unusedVariables false
            ~variable (U : Type)
            ~$predDecls
            ~$funcDecls
            ~
            ~example $varDecls
            ~$premiseDecls
            ~: $result := by
            ~$body
            ~
            ~end
            ~""".stripMargin('~')

    private def split(ss: Vector[LeanStmt], n: Int) = {
        var l = 0
        var i = 0
        var iter = ss.iterator
        while (l < n) {
            val s = iter.next()
            l = l + LeanStmt.length(s)
            i = i + 1
        }
        ss.splitAt(i)
    }

}
