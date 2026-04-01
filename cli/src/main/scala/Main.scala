package ndpc

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import com.monovore.decline.*
import ndpc.backend.*
import ndpc.frontend.*

object Main extends IOApp {
    private val inputs = Opts.arguments[String](metavar = "file").map(_.toList)

    private val check =
        Opts.subcommand("check", help = "check validity of the proof", helpFlag = true) {
            (Opts.flag("json", help = "print diagnostics in json").orFalse, inputs)
                .mapN((json, fs) => (CheckOpt(json), fs))
        }

    private val format =
        Opts.subcommand("format", help = "format proof file", helpFlag = true) {
            (
              Opts.flag("apply", help = "apply format to file instead of printing to stdout")
                  .orFalse,
              inputs
            ).mapN((apply, fs) => (FormatOpt(apply), fs))
        }

    // format: off
    private val compile =
        Opts.subcommand("compile", help = "check proof and compile to given format", helpFlag = true) {
            (
                Opts.flag(
                    "latex",
                    help = "generate latex representation of proof"
                ).as(LatexGen) orElse
                Opts.flag(
                    "typst",
                    help = "generate typst representation of proof"
                ).as(TypstGen) orElse
                Opts.flag(
                    "lean",
                    help = "generate corresponding lean proof"
                ).as(LeanGen) orElse
                (
                    Opts.flag("html", help = "generate corresponding html proof"),
                    Opts.option[java.nio.file.Path]("css", help = "custom css path", metavar = "file").map(os.Path(_)).orNone,
                ).mapN((_, css) => HtmlGen(css)),
                inputs
            ).tupled
        }
    // format: on

    private val command = Command(
      name = "ndpc",
      header = "Natural deduction proof compiler",
      helpFlag = true
    ) {
        check orElse format orElse compile
    }

    override def run(args: List[String]): IO[ExitCode] =
        command.parse(args, sys.env) match
            case Left(help) =>
                IORuntime.printHelp(help.toString).as {
                    if help.errors.isEmpty then ExitCode.Success else ExitCode.Error
                }
            case Right((runOpt, files)) =>
                runCommand(runOpt, files).map(code =>
                    if code == 0 then ExitCode.Success else ExitCode.Error
                )

    private def runCommand(runOpt: RunOpt, files: List[String]): IO[Int] =
        runOpt match
            case CheckOpt(json)   => checker.check(files, json)
            case FormatOpt(apply) => formatter.format(files, apply)
            case LatexGen         => latex.generate(files, ())
            case TypstGen         => typst.generate(files, ())
            case LeanGen          => lean.generate(files, ())
            case HtmlGen(css)     => html.generate(files, css)
}
