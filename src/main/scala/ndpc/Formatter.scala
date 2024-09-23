package ndpc
import scala.io.Source
import ndpc.Parser._
import ndpc.expr.Formula._
import parsley.Result
import ndpc.Utils._
import ndpc.expr.Rule.Tick
import parsley.{Failure, Success}
import scala.util.Try
import os.RelPath

object Formatter {
    def format(inputs: List[String], apply: Boolean): Int = {
        val results = formattedFromSource(inputs)
        val errors = results.filter(_.isFailure).asInstanceOf[List[Failure[NdpcError]]]
        val successes =
            results
                .filter(_.isSuccess)
                .map(_.get)

        if !errors.isEmpty then printErrorHuman(errors)
        var errcount = errors.length

        if apply then
            for ((dest, formatted) <- successes) do {
                try {
                    os.write.over(os.pwd / RelPath(dest), formatted)
                } catch {
                    case e =>
                        errcount = errcount + 1
                        printerrln(s"Failed to write to $dest: $e")
                }
            }
        else
            for ((_, formatted) <- successes) do {
                println(formatted)
                println()
            }
        errcount
    }

    private def formattedFromSource(
        inputs: List[String]
    ): List[Result[NdpcError, (String, String)]] =
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
                            throw new ParserException(
                              fromStringError(reason.asInstanceOf[String])
                            )
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
                            Failure(IOError(input, throwable.toString()))
                    }
                }
            }
        }

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

                var result = s"$prePadding$concl $midPadding[$rule]$comment"

                // make or elimination prettier
                if rule.isInstanceOf[Tick] then result = result + "\n"

                result
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
