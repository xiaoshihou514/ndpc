package ndpc

enum CheckOpts:
    case Default, Json, Verbose

object Checker {
    def check(input: List[String], toJson: CheckOpts): Int = ???
}
