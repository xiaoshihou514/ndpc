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

    private class ParseException(reason: String) extends Exception
    private class CheckException(reason: String) extends Exception

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
                            throw new ParseException(reason.toString())
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
                case x => {
                    println(x)
                    ???
                }
            }
        }

    private def checkOne(upf: UncheckedProof): Result[String, CheckedProof] =
        ???

    private def substitute(
        input: Line,
        lineNr: Int,
        lines: List[Line]
    ): Option[Line] = input match {
        case nonpf @ (Comment(_) | Empty()) => Some(nonpf)
        // format: off
        case it @ Pf[Int](concl, rule, _) => rule match {
            // ∧-introduction, ∧I: you have to have alrready introduced both sides
            case Rule.Intro(Introduction.And(left, right)) => 
                (lines(left), lines(right)) match {
                    case (Pf(l, _, _), Pf(r, _, _)) 
                    if (concl.equals(LFormula.And(l, r))) =>
                        Some(it.copy(rule = Rule.Intro[LF_](Introduction.And(left = l, right = r))))
                    case _ => None
                }
            // →-introduction, →I: you assume 𝝓 and prove φ
            case _ => ???
        }
        // format: on
    }
}
