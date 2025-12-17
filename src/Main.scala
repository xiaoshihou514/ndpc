package ndpc

import ndpc.frontend._
import ndpc.backend._
import cats.syntax.all._
import com.monovore.decline._
import java.nio.file.Path
import ndpc.utils.error

object Main
    extends CommandApp(
      name = "ndpc",
      header = "Natural deduction proof compiler",
      main = {
          val check =
              Opts.subcommand("check", help = "check validity of the proof", helpFlag = true) {
                  Opts.flag("json", help = "print diagnostics in json").orFalse
              }.map(CheckOpt(_))

          val format =
              Opts.subcommand("format", help = "format proof file", helpFlag = true) {
                  Opts.flag("apply", help = "apply format to file instead of printing to stdout")
                      .orFalse
              }.map(FormatOpt(_))

          // format: off
          val compile =
              Opts.subcommand("compile", help = "check proof and compile to given format", helpFlag = true) {
                Opts.flag(
                    "latex",
                    help = "generate latex representation of proof"
                ) as LatexGen orElse
                Opts.flag(
                    "typst",
                    help = "generate typst representation of proof"
                ) as TypstGen orElse
                Opts.flag(
                    "lean",
                    help = "generate corresponding lean proof"
                ) as LeanGen orElse
                (
                    Opts.flag("html", help = "generate corresponding lean proof"),
                    Opts.option[Path]("css", help = "custom css path", metavar = "file").orNone,
                ).mapN((_, css) => HtmlGen(css))
              }
          // format: on

          val inputs = Opts.arguments[String](metavar = "file").map(_.toList)

          (check orElse format orElse compile, inputs)
              .mapN[Int] {
                  case (CheckOpt(json), fs)   => checker.check(fs, json)
                  case (FormatOpt(apply), fs) => formatter.format(fs, apply)
                  case (LatexGen, fs)         => latex.generate(fs, ())
                  case (TypstGen, fs)         => ???
                  case (LeanGen, fs)          => ???
                  case (HtmlGen(css), fs)     => html.generate(fs, css)
              }
              .map(sys.exit(_))
      }
    )
