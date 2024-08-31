package ndpc

import ndpc.Checker.check
import ndpc.Assembler.compile
import ndpc.Formatter.format

object Main {
    def main(args: Array[String]): Unit = {
        val ret: Unit | Int = (args.toList) match {
            case Nil | "--help" :: _ =>
                println("""
                |Natural deduction proof compiler
                | Usage: ndpc [SUBCOMMMAND] [OPTION] [FILES]
                |
                |Arguments:
                |[FILES]      input files, use - for stdin
                |
                |SUBCOMMAND:
                |check       check proof validity
                |  --json      print diagnostics in json
                |format      format proof file
                |  --apply     apply format to file instead of printing to stdout
                |<default>   check proofs and generate pictures
                """.stripMargin)
            case "format" :: tail =>
                tail match
                    case "--apply" :: files =>
                        format(files, true)
                    case files =>
                        format(files, false)
            case "check" :: tail =>
                tail match
                    case "--json" :: files =>
                        check(tail, true)
                    case files =>
                        check(tail, false)
            case files => compile(files)
        }
        sys.exit(ret match {
            case i: Int  => i
            case _: Unit => 0
        })
    }
}
