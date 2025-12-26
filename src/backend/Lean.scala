package ndpc.backend

import ndpc.frontend.CheckedProof
import ndpc.frontend.expr.formula._
import scala.annotation.tailrec
import ndpc.frontend.parser._
import cats.syntax.all._
import ndpc.frontend.expr.rule._

private type Name = String
private type Pred = (Name, Int) // name + ary
private type Decl = (Set[Pred], Set[Name])

case class LeanExpr(val f: String, val args: List[Int]) {
    def show: String = ???
}
object LeanExpr {
    def andI(l1: Int, l2: Int) = LeanExpr("And.Intro", List(l1, l2))
}

sealed trait LeanStmt {
    def show(indent: Int): String
}
case class Intro(val l: Int) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"intro h$l"
}
case class Have(val l: Int, val ty: String, val rhs: LeanExpr) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"have h$l : $ty := ${rhs.show}"
}
case class LeanTick(val now: Int, val from: Int) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"have h$now := h$from"
}
case class Exact(val l: Int) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"exact h$l"
}
case object LeanPC extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + "apply byContradiction"
}
case class Rcases(val from: Int, val c: String, val now: Int) extends LeanStmt {
    override def show(indent: Int): String =
        " " * indent + s"rcases h$from with ⟨$c, h$now⟩"
}
case class CaseOr(
    val from: Int,
    val left: List[LeanStmt],
    val right: List[LeanStmt],
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
                val vars = concl.getVars
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
        build(pf.globals, premises.map(_.concl.asLean), compile(body), body.last.concl.asLean)
    }

    private def compile(pfs: Vector[Pf]): String = pfs.map(compile(_).show(2)).mkString("\n")

    private def compile(pf: Pf): LeanStmt = pf.rule match
        case AndIntro(left, right)                                => ???
        case ImpliesIntro(ass, res)                               => ???
        case OrIntro(either)                                      => ???
        case NotIntro(orig, bottom)                               => ???
        case DoubleNegIntro(orig)                                 => ???
        case FalsityIntro(orig, negated)                          => ???
        case TruthIntro                                           => ???
        case EquivIntro(leftImp, rightImp)                        => ???
        case ExistsIntro(orig)                                    => ???
        case ForallIntro(const, concl)                            => ???
        case AndElim(orig)                                        => ???
        case ImpliesElim(ass, imp)                                => ???
        case OrElim(or, leftAss, leftConcl, rightAss, rightConcl) => ???
        case NotElim(negated, orig)                               => ???
        case DoubleNegElim(orig)                                  => ???
        case FalsityElim(bottom)                                  => ???
        case EquivElim(equiv, either)                             => ???
        case ExistsElim(exists, ass, concl)                       => ???
        case ForallElim(orig)                                     => ???
        case ForallImpElim(ass, imp)                              => ???
        case LEM                                                  => ???
        case MT(imp, not)                                         => ???
        case PC(orig, bottom)                                     => ???
        case Refl                                                 => ???
        case EqSub(orig, eq)                                      => ???
        case Sym(orig)                                            => ???
        case ForallIConst                                         => ???
        case Given                                                => ???
        case Premise                                              => ???
        case Ass                                                  => ???
        case Tick(orig)                                           => ???

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
