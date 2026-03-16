package ndpc.frontend

import cats.effect.IO
import cats.syntax.all._
import ndpc.cliRuntime
import ndpc.utils._
import parsley.{Result, Success, Failure}

object formatter {
    def format(inputs: Seq[String], apply: Boolean): IO[Int] =
        formattedFromSource(inputs).flatMap { results =>
            val errors = results.collect { case f @ Failure(_) => f }
            val successes = results.flatten

            val printErrors =
                if errors.nonEmpty then cliRuntime.printErrorHuman(errors)
                else IO.unit

            val action =
                if apply then
                    successes.toList.foldLeftM(errors.length) { case (code, (dest, result)) =>
                        val path = os.FilePath(dest).resolveFrom(os.pwd)
                        cliRuntime.writeText(path, result).attempt.flatMap {
                            case Right(_) => IO.pure(code)
                            case Left(exception) =>
                                cliRuntime.error(s"Can't write to $dest: $exception").as(code + 1)
                        }
                    }
                else
                    successes.toList
                        .traverse_ { case (_, formatted) =>
                            cliRuntime.stdoutln(formatted) *> cliRuntime.stdoutln()
                        }
                        .as(errors.length)

            printErrors *> action
        }

    def formattedFromSource(inputs: Seq[String]): IO[Seq[Result[NdpcError, (String, String)]]] =
        inputs.toList.traverse { input =>
            cliRuntime.readInput(input).attempt.map {
                case Right(contents) =>
                    attachFile(input, formatterCore.formattedFromString(contents)) match
                        case Success(formatted) => Success((input, formatted))
                        case Failure(reason)    => Failure(reason)
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
