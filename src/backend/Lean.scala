// Typing in this file is terrible, consider refactoring
package ndpc.backend

import ndpc.frontend.CheckedProof
import ndpc.frontend.expr.formula._
import scala.annotation.tailrec
import ndpc.frontend.parser._
import cats.syntax.all._
import ndpc.frontend.pretty
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
    def ex(g: String, args: Int*) = LeanExpr(g :: args.map(x => s"h$x").toList)

    def ap(es: String*) = LeanExpr(es.map(x => s"h$x").toList)
    def andI(l1: Int, l2: Int) = ex("And.Intro", l1, l2)
    def orL(l: Int) = ex("Or.inl", l)
    def orR(l: Int) = ex("Or.inr", l)
    def ti = LeanExpr(List("True.intro"))
    def iffi(l1: Int, l2: Int) = ex("Iff.intro", l1, l2)
    def exi(name: String, l2: Int) = LeanExpr(List("Exists.intro", name, s"h$l2"))

    def andL(l: Int) = ex("And.left", l)
    def andR(l: Int) = ex("And.right", l)
    def fe(l: Int) = ex("False.elim", l)
    def mp(l1: Int, l2: Int) = LeanExpr(List(s"h$l1.mp", l2.toString))
    def em(e: String) = LeanExpr(List("em", e))
    def rfl = ex("rfl")
    def eqsub(l1: Int, l2: Int) = ex("Eq.subst", l1, l2)
    def sym(l: Int) = ex("Eq.symm", l)
}

sealed trait LeanStmt {
    def show(indent: Int): String
}
case class Intro(val ident: String) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"intro $ident"
}
object Intro {
    def line(i: Int) = Intro(s"h$i")
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
        case Forall(x, body) => s"∀ $x : Prop, (${body.asLean})"
        case Exists(x, body) => s"∃ $x : Prop, (${body.asLean})"
    }

    def diff(other: LFormula): Option[String] = {
        (f, other) match
            case (PredAp(x, Nil), PredAp(y, Nil)) if x != y => Some(y)
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
            case (PredAp(p1, args1), PredAp(p2, args2))
                if p1 == p2 && args1.length == args2.length =>
                args1.zip(args2).foldLeft(None) {
                    case (None, (a1, a2))   => a1.diff(a2)
                    case (acc @ Some(_), _) => acc
                }
            case _ => None
    }
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
        val proof = compile(pf.main, 1) :+ Exact(pfs.length)

        build(
          pf.globals,
          premises.map(_.concl.asLean),
          proof.map(_.show(2)).mkString("\n"),
          body.last.concl.asLean
        )
    }

    private type State = (
        stash: Vector[LeanStmt],
        lines: ReusableBuilder[LeanStmt, Vector[LeanStmt]],
        stashEnd: Int,
        linenr: Int
    )
    extension (s: State) {
        def clear = (Vector.empty, s.lines, s.stashEnd, s.linenr)
        def incr = (s.stash, s.lines, s.stashEnd, s.linenr + 1)
    }

    private def compile(pfs: PfScope, index: Int)(using
        lookup: Map[Int, LFormula]
    ): Vector[LeanStmt] = {
        val (s, ls, _, _) = pfs.body.foldLeft(
          (
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
                    val n = scope.flatten.collect { case _: Pf => }.length
                    (
                      acc.stash ++ compile(scope, acc.linenr),
                      acc.lines,
                      index + n,
                      acc.linenr + n
                    )
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
        println(s"$now: ${expr.pretty} ${rule}")
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
            //   intro h2
            //   have h3 : False := ...
            //   exact h3
            case NotIntro(orig, bottom) =>
                acc.lines += HaveBy(
                  now,
                  expr.asLean,
                  Intro.line(orig) +: acc.stash :+ Exact(bottom)
                )
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
                val Exists(_, ex) = expr: @unchecked
                val name = ex.diff(lookup(orig)).get
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
            case it @ OrElim(or, leftAss, leftConcl, rightAss, rightConcl) =>
                println(it)
                println("stash:")
                println(acc.stash.map(_.show(0)).mkString("\n"))
                val (ls, rs) = split(acc.stash, leftConcl - leftAss)
                println("--------------------------")
                println(ls.map(_.show(0)).mkString("\n"))
                println("--------------------------")
                println(rs.map(_.show(0)).mkString("\n"))
                println("--------------------------")
                acc.lines += HaveBy(
                  now,
                  expr.asLean,
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
                acc.lines += Have(
                  now.toString,
                  expr.asLean,
                  LeanExpr.mp(equiv, either)
                )
                acc

            // have h : C := by
            //   rcases h1 with ⟨x, hx⟩ -- inserted
            //   have hC : C := h2 x hx
            //   exact hC
            case ExistsElim(exists, ass, concl) =>
                val Exists(_, ex) = lookup(exists): @unchecked
                val name = ex.diff(lookup(ass)).get
                acc.lines += HaveBy(
                  now,
                  expr.asLean,
                  Rcases(exists, name, ass) +: acc.stash :+ Exact(concl)
                )
                acc.clear

            // have hPA : P A := h A
            case ForallElim(orig) =>
                val Forall(_, fa) = lookup(orig): @unchecked
                val name = fa.diff(expr).get
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
                val Forall(x, impBody) = lookup(imp): @unchecked
                val name = (expr.names -- (impBody.names - x)).head
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
                      +: Intro.line(orig) // TODO
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
                acc.lines += Intro(expr.asLean)
                acc

            // do nothing, assume place of use will fill it in
            case Ass => acc
            case Tick(orig) =>
                acc.lines += Have(now.toString, expr.asLean, LeanExpr(List(s"h$orig")))
                acc
            case Given | Premise => acc
    }

    private def ty(arity: Int): String =
        if arity == 0 then "Prop" else s"Prop → ${ty(arity - 1)}"

    private def build(decl: Decl, premises: Vector[String], body: String, result: String) =
        val (preds, vars) = decl
        val predDecls = preds.map((f, n) => s"axiom $f : ${ty(n)}").mkString("\n")
        val varDecls = vars.mkString(" ")
        val premiseDecls = premises.zipWithIndex.map((p, n) => s"  (h${n + 1} : $p)").mkString("\n")
        s"""-- `lean *.lean` or https://live.lean-lang.org/
            ~section
            ~open Classical
            ~set_option linter.unusedVariables false
            ~$predDecls
            ~
            ~example {$varDecls : Prop}
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
            l = l + length(s)
            i = i + 1
        }
        ss.splitAt(i)
    }

    private def length(s: LeanStmt): Int = s match
        case HaveBy(l, ty, rhs) => rhs.map(length).sum
        case CaseOr(from, left, right, rangeL, rangeR) =>
            1 + (rangeL._2 - rangeL._1) + (rangeR._2 - rangeR._1)
        case _ => 1

}
