package ndpc.cli.frontend

import cats.effect.IO
import cats.syntax.all.*
import ndpc.cli.{CliRuntime, IORuntime}
import ndpc.utils.*
import parsley.{Result, Success, Failure}
import ndpc.frontend.{CheckedProof, Checker}

object checker {
    def check(inputs: List[String], toJson: Boolean, runtime: CliRuntime = IORuntime): IO[Int] =
        pfFromSource(inputs, runtime).flatMap { results =>
            val errors = results.collect { case f @ Failure(_) => f }
            val output =
                if errors.nonEmpty then
                    if toJson then runtime.printErrorJson(errors)
                    else runtime.printErrorHuman(errors)
                else runtime.ok("All proofs are valid!")
            output.as(errors.length)
        }

    def pfFromSource(
        inputs: List[String],
        runtime: CliRuntime = IORuntime
    ): IO[Seq[Result[NdpcError, CheckedProof]]] =
        inputs.traverse { input =>
            runtime.readInput(input).attempt.map {
                case Right(contents) =>
                    attachFile(input, Checker.checkedFromString(contents))
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
