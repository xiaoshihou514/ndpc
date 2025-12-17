package ndpc.frontend

import ndpc.frontend.checker.check
import ndpc.backend.codegen.generate
import ndpc.frontend.formatter.format
import ndpc.utils.error

object Main {
    def main(args: Array[String]): Unit =
        (args.toList) match {
            case Nil | "--help" :: _ =>
                println("""
                |Natural deduction proof compiler
                | Usage: ndpc [SUBCOMMAND] [OPTION] [FILES]
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
                    case "--apply" :: (files @ (_ :: _)) =>
                        format(files, true)
                    case (files @ (_ :: _)) =>
                        format(files, false)
                    case _ =>
                        error("Error: missing targets")
            case "check" :: tail =>
                tail match
                    case "--json" :: (files @ (_ :: _)) =>
                        check(files, true)
                    case (files @ (_ :: _)) =>
                        check(files, false)
                    case _ =>
                        error("Error: missing targets")
            case "--css" :: css :: (files @ (_ :: _)) =>
                generate(files, Some(css))
            case "--css" :: css :: Nil =>
                error("Error: missing targets")
            case "--css" :: Nil =>
                error("Error: missing --css argument and targets")
            case files =>
                generate(files, None)
        } match {
            case i: Int  => sys.exit(i)
            case _: Unit => sys.exit(0)
        }
}
