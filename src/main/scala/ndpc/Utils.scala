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

    def ok(x: Any) = println(s"$GREEN$x$RESET")
    def error(x: Any) = println(s"$FAIL$x$RESET")
}
