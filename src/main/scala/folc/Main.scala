package folc
import folc.Checker.check
import folc.Renderer.compile

object Main {
    def main(args: Array[String]): Unit = {
        val ret: Unit | Int = args match {
            case Array() | Array("--help", _*) =>
                println(
                  """
                  First logic compiler
                     Usage: folc [OPTION] [FILE]...

                  Arguments:
                    [FILE]...   input files, use - for stdin

                  Options:
                    -c          only check
                    <default>   generate pretty printed boxes
                  """.trim()
                )
            case Array("-c", tail @ _*) =>
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
