package ndpc

import ndpc.Checker.check
import ndpc.CheckOpts
import ndpc.Assembler.compile
import ndpc.Formatter.format

// format: off
val help =
"""
Natural deduction proof compiler
 Usage: ndpc [SUBCOMMMAND] [OPTION] [FILE]

Arguments:
[FILE]      input files, use - for stdin

SUBCOMMAND:
check       check proof validity
format      format proof file
<default>   generate proof with pretty printed boxes

Options:
--apply     apply format to file instead of printing to stdout
--json      print diagnostics in json
--verbose   print verbose diagnostics
"""
// format: on

object Main {
    def main(args: Array[String]): Unit = {
        val ret: Unit | Int = (args.toList) match {
            case Nil | "--help" :: _ =>
                println(help)
            case "format" :: tail =>
                tail match
                    case "--apply" :: files =>
                        format(files, true)
                    case files =>
                        format(files, false)
            case "check" :: tail =>
                tail match
                    case "--json" :: files =>
                        check(tail, CheckOpts.Json)
                    case "--verbose" :: files =>
                        check(tail, CheckOpts.Verbose)
                    case files =>
                        check(tail, CheckOpts.Default)
            case files => compile(files)
        }
        sys.exit(ret match {
            case i: Int  => i
            case _: Unit => 0
        })
    }
}
