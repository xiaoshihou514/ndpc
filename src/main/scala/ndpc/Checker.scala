package ndpc

import ndpc.Parser._
import ndpc.expr.Rule._
import ndpc.expr.Formula._

import scala.io.Source
import scala.util.Try
import parsley.{Success, Failure}
import parsley.Result

case class CheckedProof(main: PfScope)

object Checker {
    // just controller exceptions that can be pattern matched
    private class ParserException(val reason: String) extends Exception
    private object ParserException:
        def unapply(e: ParserException): Option[String] = Some(e.reason)
    private class CheckException(val reason: String) extends Exception
    private object CheckException:
        def unapply(e: CheckException): Option[String] = Some(e.reason)

    private sealed trait CheckError
    private case class IOError(reason: String) extends CheckError
    private case class SyntaxError(reason: String) extends CheckError
    private case class SemanticsError(reason: String) extends CheckError

    def check(inputs: List[String], toJson: Boolean): Int =
        val errors = pfFromSource(inputs).filter {
            (out: Result[CheckError, CheckedProof]) =>
                out match {
                    case Left(_) => false
                    case _       => true
                }
        }
        if toJson then
            println(buildErrorJson(errors.asInstanceOf[List[CheckError]]))
        else println(buildErrorHuman(errors.asInstanceOf[List[CheckError]]))
        errors.length

    private def buildErrorHuman(errors: List[CheckError]): String = ???

    private def buildErrorJson(errors: List[CheckError]): String = ???

    // I really want consistent error handling here so I went for java exceptions,
    // which is well captured by scala.util.Try
    private def pfFromSource(
        inputs: List[String]
    ): List[Result[CheckError, CheckedProof]] =
        inputs.map { (input: String) =>
            Try(input)
                .map { (i: String) =>
                    val src = i match {
                        case "-"  => Source.stdin
                        case file => Source.fromFile(file)
                    }
                    src.mkString
                }
                .map { (contents: String) =>
                    parse(contents) match {
                        case Success(ast) => ast
                        case Failure(reason) =>
                            throw new ParserException(reason.toString())
                    }
                }
                .map { (upf: UncheckedProof) =>
                    checkOne(upf) match {
                        case Success(pf) => pf
                        case Failure(reason) =>
                            throw new CheckException(reason.toString())
                    }
                } match {
                case scala.util.Success(pf) => Success(pf)
                case scala.util.Failure(exception) => {
                    exception match {
                        case ParserException(reason) =>
                            Failure(SyntaxError(reason))
                        case CheckException(reason) =>
                            Failure(SemanticsError(reason))
                        case throwable @ _ =>
                            Failure(IOError(throwable.toString()))
                    }
                }
            }
        }

    private def checkOne(upf: UncheckedProof): Result[String, CheckedProof] =
        ???

    private def trySubstitute(
        input: Line,
        lineNr: Int,
        lines: List[Line]
    ): Result[String, Line] = input match {
        case nonpf @ (Comment(_) | Empty()) => Success(nonpf)
        // format: off
        case it @ Pf[Int](concl, rule, _) => rule match {
            // ∧-introduction, ∧I: you have to have already introduced both sides
        case Rule.Intro(Introduction.And(left, right)) => 
            trySubstituteAndIntroduction(it, lineNr, lines, concl, left, right)

        // →-introduction, →I: you assume 𝝓 and prove φ
        case Rule.Intro(Introduction.Implies(left, right))
        if left < lines.length && right < lines.length => 
            (lines(left - 1), lines(right - 1)) match {
                case (Pf(l, _, _), Pf(r, _, _)) 
                if (concl.equals(LFormula.Implies(l, r))) =>
                    Success(it.copy(rule = Rule.Intro[LF_](Introduction.Implies(l, r))))
                case (l, r) => 
                    Failure(s"line $lineNr: rule \"Implies Introduction\" expects lhs ($l) ^ rhs ($r) equals $concl")
            }
        case Rule.Intro(Introduction.Implies(_, _)) => 
            Failure(s"line $lineNr: $rule specified line numbers that's out of bound")

        // ∨-introduction, ∨I: prove either side
        case Rule.Intro(Introduction.Or(leftOrRight))
        if leftOrRight < lines.length => 
            lines(leftOrRight - 1) match {
                case Pf(pf, _, _) =>
                    concl match {
                        case LFormula.Or(l, r)
                        if leftOrRight.equals(l) || leftOrRight.equals(r) =>
                            Success(it.copy(rule = Rule.Intro[LF_](Introduction.Or(pf))))
                        case _ =>
                            Failure(s"line $lineNr: rule \"Or Introduction\" expects conclusion ($concl) is an or expression with reference on the lhs ($pf | a) or on the rhs (a | $pf)")
                    }
                case (l, r) => 
                    Failure(s"line $lineNr: rule \"Implies Introduction\" expects lhs ($l) ^ rhs ($r) equals $concl")
            }
        case Rule.Intro(Introduction.Or(_)) => 
            Failure(s"line $lineNr: $rule specified line numbers that's out of bound")
        }
        // format: on
    }

    private def trySubstituteAndIntroduction(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line],
        concl: LF_,
        left: Int,
        right: Int
    ): Result[String, Line] =
        if left < lines.length && right < lines.length then
            (lines(left - 1), lines(right - 1)) match {
                case (Pf(l, _, _), Pf(r, _, _))
                    if (concl.equals(LFormula.And(l, r))) =>
                    Success(
                      input.concl.copy(rule = Rule.Intro[LF_](Introduction.And(l, r)))
                    )
                case (l, r) =>
                    Failure(
                      s"line $lineNr: rule \"And Introduction\" expects lhs ($l) ^ rhs ($r) equals $concl"
                    )
            }
        else
            Failure(
              s"line $lineNr: ${input.rule} specified line numbers that's out of bound"
            )

}
