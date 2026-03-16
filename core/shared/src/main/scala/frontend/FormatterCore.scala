package ndpc.frontend

import ndpc.frontend.expr.formula.*
import ndpc.frontend.expr.rule.Tick
import ndpc.frontend.parser.*
import ndpc.frontend.pretty.*
import ndpc.utils.*
import parsley.{Result, Success, Failure}

object formatterCore {
    def formattedFromString(contents: String): Result[NdpcError, String] =
        parse(contents) match
            case Success(ast)    => Success(formatPure(ast))
            case Failure(reason) => Failure(SyntaxError(reason))

    private def findReasonAlign(target: PfScope, iniIndent: Int = 0): Int =
        2 * iniIndent + target.body.map {
            case Right(s @ PfScope(_)) => findReasonAlign(s, iniIndent + 1)
            case Left(Pf(concl, _, _)) => concl.pretty.length()
            case _                     => 0
        }.max

    private def formatLine(line: Line, indent: Int, reasonAlign: Int): String =
        line match {
            case Empty             => ""
            case Comment(contents) => " ".repeat(indent * 2) + s"-- $contents"
            case Pf(concl, rule, c) =>
                val comment = c match
                    case None                    => ""
                    case Some(Comment(contents)) => s" -- $contents"
                val prePadding = " ".repeat(indent * 2)
                val midPadding = " ".repeat(reasonAlign - concl.pretty.length - indent * 2)
                val result = s"$prePadding${concl.pretty} $midPadding[${rule.pretty}]$comment"
                rule match
                    case Tick(_) => result + "\n"
                    case _       => result
        }

    def formatScope(target: PfScope, currentIndent: Int, reasonAlign: Int): String =
        target.body
            .filterNot {
                case Left(Empty) => true
                case _           => false
            }
            .map {
                case Right(s @ PfScope(_)) => formatScope(s, currentIndent + 1, reasonAlign)
                case Left(l)               => formatLine(l, currentIndent, reasonAlign)
            }
            .mkString("\n")

    def formatPure(target: UncheckedProof): String =
        formatScope(target.main, 0, findReasonAlign(target.main))
}
