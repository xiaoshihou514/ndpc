package ndpc
import ndpc.Checker.check
import ndpc.Renderer.compile
import ndpc.Formatter.format

object Main {
    def main(args: Array[String]): Unit = {
        val ret: Unit | Int = args match {
            case Array() | Array("--help", _*) =>
                println(
                  """
                  First logic compiler
                     Usage: ndpc [OPTION] [FILE]...

                  Arguments:
                    [FILE]...   input files, use - for stdin

                  Options:
                    check       check proof validity
                    fmt         format proof file
                    <default>   generate pretty printed boxes
                  """.trim()
                )
            case Array("fmt", tail @ _*) =>
                format(tail.toList)
            case Array("check", tail @ _*) =>
                check(tail.toList)
            case Array(it @ _*) =>
                compile(it.toList)
        }
        sys.exit(ret match {
            case i: Int  => i
            case _: Unit => 0
        })
    }
}
