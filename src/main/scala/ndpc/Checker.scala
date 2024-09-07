package ndpc

import ndpc.Parser._
import ndpc.expr.Rule._
import ndpc.expr.Formula._

import scala.io.Source
import scala.util.Try
import parsley.{Success, Failure}
import parsley.Result

case class CheckedProof(main: PfScope[LF_])

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
                    case Success(_) => false
                    case _          => true
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
        input: Line[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        env: Set[String]
    ): Result[String, Line[_]] = input match {
        case nonpf @ (Comment(_) | Empty()) => Success(nonpf)
        // format: off
        case it @ Pf[Int](concl, rule, _) => rule match {
        case AndIntro(left, right) => 
            trySubstituteAndIntro(it, lineNr, lines, concl, left, right)
        case ImpliesIntro(ass, res) =>
            trySubstituteImpliesIntro(it, lineNr, lines, concl, ass, res)
        case OrIntro(either) =>
            trySubstituteOrIntro(it, lineNr, lines, concl, either)
        case NotIntro(orig, bottom) =>
            trySubstituteNotIntro(it, lineNr, lines, concl, orig, bottom)
        case DoubleNegIntro(orig) =>
            trySubstituteDoubleNegIntro(it, lineNr, lines, concl, orig)
        case FalsityIntro(orig, negated) =>
            ???
        case TruthIntro() =>
            ???
        case EquivIntro(leftImp, rightImp) =>
            ???
        case ExistsIntro(orig) =>
            ???
        case ForallIntro(orig, concl) =>
            ???
        case _ => ???
        }
        // format: on
    }

    private def outOfBound(currLine: Int, rule: Rule[_]) =
        Failure(
          s"line $currLine: $rule specified line numbers that's out of bound"
        )

    private def trySubstituteAndIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        left: Int,
        right: Int
    ): Result[String, Line[LF_]] =
        if left < lines.length && right < lines.length then
            (lines(left - 1), lines(right - 1)) match {
                case (Pf(l, _, _), Pf(r, _, _)) if (concl == And(l, r)) =>
                    Success(
                      input.copy(rule = AndIntro(l, r))
                    )
                case (l, r) =>
                    Failure(
                      s"line $lineNr: rule ${input.rule} expects lhs ^ rhs \"($l) ^ ($r)\" equals $concl"
                    )
            }
        else outOfBound(lineNr, input.rule)

    private def trySubstituteImpliesIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        imp: Int,
        res: Int
    ): Result[String, Line[LF_]] =
        if imp < lines.length && res < lines.length then
            (lines(imp - 1), lines(res - 1)) match {
                case (Pf(i, _, _), Pf(r, _, _)) if (concl == Implies(i, r)) =>
                    Success(
                      input.copy(rule = ImpliesIntro(i, r))
                    )
                case (l, r) =>
                    Failure(
                      s"line $lineNr: rule ${input.rule} expects lhs -> rhs \"($l) -> ($r)\" equals $concl"
                    )
            }
        else outOfBound(lineNr, input.rule)

    private def trySubstituteOrIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        either: Int
    ): Result[String, Line[LF_]] =
        if either < lines.length then
            (lines(either - 1), concl) match {
                case (Pf(lf, _, _), Or(l, r)) if lf == l || lf == r =>
                    Success(input.copy(rule = OrIntro(lf)))
                case (e, pf) =>
                    Failure(
                      s"line $lineNr: rule ${input.rule} expects $pf equals x / y or y / x, where x is $e"
                    )
            }
        else outOfBound(lineNr, input.rule)

    private def trySubstituteNotIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        orig: Int,
        bottom: Int
    ): Result[String, Line[LF_]] =
        if orig < lines.length && bottom < lines.length then
            (lines(orig - 1), lines(bottom - 1)) match {
                case (Pf(o, _, _), Pf(b, _, _))
                    if (b == Falsity() && concl == Not(o)) =>
                    Success(
                      input.copy(rule = NotIntro(o, b))
                    )
                case (l, r) =>
                    Failure(
                      s"line $lineNr: rule ${input.rule} expects $concl equals ~($orig) and $bottom is F"
                    )
            }
        else outOfBound(lineNr, input.rule)

    private def trySubstituteDoubleNegIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        orig: Int
    ): Result[String, Line[LF_]] =
        if orig < lines.length then
            lines(orig - 1) match {
                case Pf(o, _, _) if concl == Not(Not(o)) =>
                    Success(input.copy(rule = DoubleNegIntro(o)))
                case l =>
                    Failure(
                      s"line $lineNr: rule ${input.rule} expects $concl equals ~~($l)"
                    )
            }
        else outOfBound(lineNr, input.rule)
}
