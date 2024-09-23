package ndpc
import scala.io.Source
import ndpc.Parser._
import ndpc.expr.Formula._
import parsley.Result
import ndpc.Utils._

object Formatter {
    def format(input: List[String], apply: Boolean): Int = 0

    private def astFromSource(
        inputs: List[String]
    ): List[Result[IOError | SyntaxError, UncheckedProof]] =
        ???

    private def findReasonAlign(target: PfScope, ini_indent: Int = 0): Int =
        2 * ini_indent + target.body
            .map(_ match {
                case s @ PfScope(_)  => findReasonAlign(s, ini_indent + 1)
                case Pf(concl, _, _) => concl.toString().length()
                case _               => 0
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
                val midPadding = " ".repeat(reasonAlign - concl.toString().length() - indent * 2)

                s"$prePadding$concl $midPadding[$rule]$comment"
        }

    def formatScope(target: PfScope, currentIndent: Int, reasonAlign: Int): String =
        target.body
            .map((line) =>
                line match {
                    case s @ PfScope(_) => formatScope(s, currentIndent + 1, reasonAlign)
                    case l => formatLine(l.asInstanceOf[Line], currentIndent, reasonAlign)
                }
            )
            .mkString("\n")

    def formatPure(target: UncheckedProof): String =
        formatScope(target.main, 0, findReasonAlign(target.main))
}
