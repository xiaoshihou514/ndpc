package ndpc

import ndpc.Parser._
import ndpc.expr.Formula._
import parsley.Result
import ndpc.Utils._
import ndpc.expr.Rule.Tick

import scala.io.Source
import scala.util.Try
import os.{RelPath, Path}
import parsley.{Failure, Success}

object Formatter {
    def format(inputs: Seq[String], apply: Boolean): Int = {
        val results = formattedFromSource(inputs)
        val errors = results.collect { case f @ Failure(_) => f }
        val successes = results.flatten

        if !errors.isEmpty then printErrorHuman(errors)

        if apply then
            var code = errors.length

            for ((dest, result) <- successes) do {
                val path = os.FilePath(dest).resolveFrom(os.pwd)
                Try(os.write.over(path, result)) match
                    case _: scala.util.Failure[_] => code = code + 1
                    case _                        =>
            }

            code
        else
            for ((_, formatted) <- successes) do {
                println(formatted)
                println()
            }
            errors.length
    }

    def formattedFromSource(
        inputs: Seq[String]
    ): Seq[Result[NdpcError, (String, String)]] =
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
                            throw new ParserException(fromStringError(s"$reason"))
                    }
                }
                .map(formatPure.andThen((input, _))) match {
                case scala.util.Success(res) => Success(res)
                case scala.util.Failure(exception) => {
                    exception match {
                        case ParserException(reason) =>
                            Failure(
                              SyntaxError(reason.copy(file = Some(input)))
                            )
                        case throwable @ _ =>
                            Failure(IOError(input, s"$throwable"))
                    }
                }
            }
        }

    private def findReasonAlign(target: PfScope, ini_indent: Int = 0): Int =
        2 * ini_indent + target.body
            .map(_ match {
                case Right(s @ PfScope(_)) => findReasonAlign(s, ini_indent + 1)
                case Left(Pf(concl, _, _)) => concl.toString().length()
                case _                     => 0
            })
            .max

    private def formatLine(line: Line, indent: Int, reasonAlign: Int): String =
        line match {
            case Empty()           => ""
            case Comment(contents) => " ".repeat(indent * 2) + s"-- $contents"
            case Pf(concl, rule, c) =>
                val comment = c match
                    case None                    => ""
                    case Some(Comment(contents)) => s" -- $contents"
                val prePadding = " ".repeat(indent * 2)
                val midPadding = " ".repeat(reasonAlign - concl.toString.length - indent * 2)

                val result = s"$prePadding$concl $midPadding[$rule]$comment"

                // make or elimination prettier
                rule match
                    case Tick(_) => result + "\n"
                    case _       => result
        }

    def formatScope(target: PfScope, currentIndent: Int, reasonAlign: Int): String =
        target.body
            .filterNot(_ match {
                case Left(Empty()) => true
                case _             => false
            })
            .map((line) =>
                line match {
                    case Right(s @ PfScope(_)) => formatScope(s, currentIndent + 1, reasonAlign)
                    case Left(l)               => formatLine(l, currentIndent, reasonAlign)
                }
            )
            .mkString("\n")

    def formatPure(target: UncheckedProof): String =
        formatScope(target.main, 0, findReasonAlign(target.main))
}
