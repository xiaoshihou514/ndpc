package ndpc

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
    sealed trait CheckError
    case class IOError(file: String, reason: String) extends CheckError
    case class SyntaxError(reason: EnrichedErr) extends CheckError
    case class SemanticsError(reason: EnrichedErr) extends CheckError

    case class EnrichedErr(exp: String, file: Option[String], location: Option[Int]):
        def toJson(): String =
            s"""
            |{
            |    "line": ${location.get},
            |    "explanation": "${exp.replace("\n", "\\n")}"
            |}
            """.stripMargin
}
