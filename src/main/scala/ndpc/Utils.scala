package ndpc

import parsley.{Failure, Success}

object Utils {
    val HEADER = "\u001B[95m"
    val BLUE = "\u001B[94m"
    val CYAN = "\u001B[96m"
    val GREEN = "\u001B[92m"
    val WARN = "\u001B[93m"
    val FAIL = "\u001B[31m"
    val RESET = "\u001B[0m"
    val BOLD = "\u001B[1m"
    val UNDERLINE = "\u001B[4m"

    def printerrln(x: Any) = System.err.println(x)
    def printerrln() = System.err.println
    def ok(x: Any) = System.err.println(s"$GREEN$x$RESET")
    def error(x: Any) = println(s"$FAIL$x$RESET")

    // just controller exceptions that can be pattern matched
    case class ParserException(val reason: EnrichedErr) extends Exception
    object ParserException:
        def unapply(e: ParserException): Option[EnrichedErr] = Some(e.reason)
    case class CheckException(val reason: EnrichedErr) extends Exception
    object CheckException:
        def unapply(e: CheckException): Option[EnrichedErr] = Some(e.reason)

    // just wrappers, maybe java people have a fancier name for this
    sealed trait NdpcError
    case class IOError(file: String, reason: String) extends NdpcError
    case class SyntaxError(reason: EnrichedErr) extends NdpcError
    case class SemanticsError(reason: EnrichedErr) extends NdpcError

    case class EnrichedErr(exp: String, file: Option[String], location: Option[Int]):
        def toJson(): String =
            s"""
            |{
            |  "file": "${file.get}",
            |  "line": ${location.get},
            |  "explanation": "${exp.replace("\n", "\\n")}"
            |}
            """.stripMargin

    def fromStringError(errDesc: String): EnrichedErr =
        EnrichedErr(
          errDesc,
          Some("sdjal"),
          Some(232)
        )

    def printErrorHuman(errors: List[Failure[NdpcError]]) = {
        for (e <- errors) do {
            e.msg match {
                case IOError(file, reason) =>
                    error(s"Can't read from $file: $reason")
                case SyntaxError(reason) =>
                    printerrln(s"${FAIL}Syntax error${RESET}:")
                    printerrln(
                      s"${BOLD}${reason.file.get}${RESET}, line ${reason.location.get}:"
                    )
                    printerrln(reason.exp)
                case SemanticsError(reason) =>
                    printerrln(s"${FAIL}Semantics error${RESET}:")
                    printerrln(
                      s"${BOLD}${reason.file.get}${RESET}, line ${reason.location.get}:"
                    )
                    printerrln(reason.exp)
            }
            printerrln()
        }
    }

    def printErrorJson(errors: List[Failure[NdpcError]]) =
        for (e <- errors) do {
            e.msg match {
                case IOError(file, reason) =>
                    printerrln(s"Can't read from $file: $reason")
                case SyntaxError(reason) =>
                    println(reason.toJson())
                case SemanticsError(reason) =>
                    println(reason.toJson())
            }
        }
}
