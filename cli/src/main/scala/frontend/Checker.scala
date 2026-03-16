package ndpc.frontend

import cats.effect.IO
import cats.syntax.all._
import ndpc.cliRuntime
import ndpc.utils._
import parsley.{Result, Success, Failure}

object checker {
    def check(inputs: Seq[String], toJson: Boolean): IO[Int] =
        pfFromSource(inputs).flatMap { results =>
            val errors = results.collect { case f @ Failure(_) => f }
            val output =
                if errors.nonEmpty then
                    if toJson then cliRuntime.printErrorJson(errors)
                    else cliRuntime.printErrorHuman(errors)
                else cliRuntime.ok("All proofs are valid!")
            output.as(errors.length)
        }

    def pfFromSource(inputs: Seq[String]): IO[Seq[Result[NdpcError, CheckedProof]]] =
        inputs.toList.traverse { input =>
            cliRuntime.readInput(input).attempt.map {
                case Right(contents) =>
                    attachFile(input, checkerCore.checkedFromString(contents))
                case Left(exception) =>
                    Failure(IOError(input, exception.toString))
            }
        }

    private def inputFile(input: String): Option[String] =
        Option.when(input != "-")(input)

    private def attachFile(
        input: String,
        result: Result[NdpcError, CheckedProof]
    ): Result[NdpcError, CheckedProof] =
        result match
            case Success(value) => Success(value)
            case Failure(SyntaxError(reason)) =>
                Failure(SyntaxError(reason.copy(file = inputFile(input))))
            case Failure(SemanticsError(reason)) =>
                Failure(SemanticsError(reason.copy(file = inputFile(input))))
            case failure @ Failure(_) => failure
}
