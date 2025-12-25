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
    // def premises: (Map[Int, Name], List[LFormula]) = ???
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

    private def compile(pf: Vector[Pf]): String = ???

    @tailrec private def ty(arity: Int): String =
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
