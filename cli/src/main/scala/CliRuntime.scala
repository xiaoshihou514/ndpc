package ndpc

import cats.effect.IO
import cats.syntax.all._
import ndpc.utils._
import parsley.Failure

import java.nio.file.Path
import scala.io.Source

object cliRuntime {
    private val GREEN = "\u001B[92m"
    private val FAIL = "\u001B[31m"
    private val RESET = "\u001B[0m"
    private val BOLD = "\u001B[1m"

    def stderrln(x: Any): IO[Unit] = IO.blocking(System.err.println(x))
    def stderrln(): IO[Unit] = stderrln("")
    def stdoutln(x: Any): IO[Unit] = IO.blocking(System.out.println(x))
    def stdoutln(): IO[Unit] = stdoutln("")

    def ok(x: Any): IO[Unit] = stderrln(s"$GREEN$x$RESET")
    def error(x: Any): IO[Unit] = stderrln(s"$FAIL$x$RESET")
    def printHelp(help: String): IO[Unit] = stderrln(help)

    def printErrorHuman(errors: Seq[Failure[? <: NdpcError]]): IO[Unit] =
        errors.toList.traverse_(printFailureHuman)

    def printErrorJson(errors: Seq[Failure[? <: NdpcError]]): IO[Unit] =
        errors.toList.traverse_ { e =>
            e.msg match
                case IOError(file, reason) => stderrln(s"Can't read from $file: $reason")
                case SyntaxError(reason)   => stdoutln(reason.toJson())
                case SemanticsError(reason) => stdoutln(reason.toJson())
        }

    def readInput(input: String): IO[String] =
        IO.blocking {
            val src = input match
                case "-"  => Source.stdin
                case file => Source.fromFile(file)
            try src.getLines.mkString("\n")
            finally src.close()
        }

    def writeText(path: os.Path, contents: String): IO[Unit] =
        IO.blocking(os.write.over(path, contents))

    def readPath(path: Path): IO[String] =
        IO.blocking(os.read(os.Path(path)))

    private def printFailureHuman(failure: Failure[? <: NdpcError]): IO[Unit] =
        val io = failure.msg match
            case IOError(file, reason) =>
                error(s"Can't read from $file: $reason")
            case SyntaxError(reason) =>
                stderrln(s"${FAIL}Syntax error${RESET}:") *>
                stderrln(s"$BOLD${reason.file.getOrElse("<stdin>")}$RESET, ${reason.location}:") *>
                    stderrln(reason.exp)
            case SemanticsError(reason) =>
                stderrln(s"${FAIL}Semantics error${RESET}:") *>
                    stderrln(s"$BOLD${reason.file.getOrElse("<stdin>")}$RESET, ${reason.location}:") *>
                    stderrln(reason.exp)
        io *> stderrln()
}
