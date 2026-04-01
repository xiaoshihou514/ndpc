package ndpc.frontend

import cats.effect.IO
import cats.syntax.all.*
import ndpc.{CliRuntime, IORuntime}
import ndpc.utils.*
import parsley.{Result, Success, Failure}

object formatter {
    def format(inputs: Seq[String], apply: Boolean, runtime: CliRuntime = IORuntime): IO[Int] =
        formattedFromSource(inputs, runtime).flatMap { results =>
            val errors = results.collect { case f @ Failure(_) => f }
            val successes = results.collect { case Success(value) => value }

            val printErrors =
                if errors.nonEmpty then runtime.printErrorHuman(errors)
                else IO.unit

            val action =
                if apply then
                    successes.toList.foldLeftM(errors.length) { case (code, (dest, result)) =>
                        val path = os.FilePath(dest).resolveFrom(os.pwd)
                        runtime.writeText(path, result).attempt.flatMap {
                            case Right(_) => IO.pure(code)
                            case Left(exception) =>
                                runtime.error(s"Can't write to $dest: $exception").as(code + 1)
                        }
                    }
                else
                    successes.toList
                        .traverse_ { case (_, formatted) =>
                            runtime.stdoutln(formatted) *> runtime.stdoutln()
                        }
                        .as(errors.length)

            printErrors *> action
        }

    def formattedFromSource(
        inputs: Seq[String],
        runtime: CliRuntime = IORuntime
    ): IO[Seq[Result[NdpcError, (String, String)]]] =
        inputs.toList.traverse { input =>
            runtime.readInput(input).attempt.map {
                case Right(contents) =>
                    attachFile(input, Formatter.formattedFromString(contents))
                        .map((input, _))
                case Left(exception) =>
                    Failure(IOError(input, exception.toString))
            }
        }

    private def inputFile(input: String): Option[String] =
        Option.when(input != "-")(input)

    private def attachFile(
        input: String,
        result: Result[NdpcError, String]
    ): Result[NdpcError, String] =
        result match
            case Success(value) => Success(value)
            case Failure(SyntaxError(reason)) =>
                Failure(SyntaxError(reason.copy(file = inputFile(input))))
            case failure @ Failure(_) => failure
}
