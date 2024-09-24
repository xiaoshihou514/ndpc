package ndpc

import ndpc.Checker.check
import ndpc.Compiler.compile
import ndpc.Formatter.format
import ndpc.Utils.error

object Main {
    def main(args: Array[String]): Unit =
        (args.toList) match {
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
                |<default>   check proofs and generate html
                |  --css       use custom css styling for generated html
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
                        check(files, true)
                    case files =>
                        check(files, false)
            case "--css" :: css :: files =>
                compile(files, Some(css))
            case "--css" :: Nil =>
                error("Error: missing --css argument and targets")
            case files =>
                compile(files, None)
        } match {
            case i: Int  => sys.exit(i)
            case _: Unit => sys.exit(0)
        }
}
