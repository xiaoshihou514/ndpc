package ndpc

import cats.effect.{IO, Ref}
import ndpc.utils.*
import parsley.Failure

import java.io.FileNotFoundException

extension (ss: Vector[String]) def text = ss.mkString("\n")

case class TestCliRuntimeState(
    stdout: Vector[String] = Vector.empty,
    stderr: Vector[String] = Vector.empty,
    writes: Vector[(os.Path, String)] = Vector.empty,
    inputs: Map[String, String] = Map.empty,
    pathReads: Map[os.Path, Either[Throwable, String]] = Map.empty
)

class TestCliRuntime private (ref: Ref[IO, TestCliRuntimeState]) extends CliRuntime {
    override def stderrln(x: Any): IO[Unit] =
        ref.update(state => state.copy(stderr = state.stderr :+ x.toString))

    override def stdoutln(x: Any): IO[Unit] =
        ref.update(state => state.copy(stdout = state.stdout :+ x.toString))

    override def readInput(input: String): IO[String] =
        ref.get.flatMap { state =>
            state.inputs.get(input) match
                case Some(contents) => IO.pure(contents)
                case None           => IO.raiseError(new FileNotFoundException(input))
        }

    override def writeText(path: os.Path, contents: String): IO[Unit] =
        ref.update(state => state.copy(writes = state.writes :+ (path -> contents)))

    override def readPath(path: os.Path): IO[String] =
        ref.get.flatMap { state =>
            state.pathReads.get(path) match
                case Some(Right(contents)) => IO.pure(contents)
                case Some(Left(error))     => IO.raiseError(error)
                case None => IO.raiseError(new FileNotFoundException(path.toString))
        }

    override def ok(x: Any): IO[Unit] = stderrln(x)
    override def error(x: Any): IO[Unit] = stderrln(x)

    override protected def printFailureHuman(failure: Failure[? <: NdpcError]): IO[Unit] =
        val io = failure.msg match
            case IOError(file, reason) =>
                error(s"Can't read from $file: $reason")
            case SyntaxError(reason) =>
                stderrln("Syntax error:") *>
                    stderrln(s"${reason.file.getOrElse("<stdin>")}, ${reason.location}:") *>
                    stderrln(reason.exp)
            case SemanticsError(reason) =>
                stderrln("Semantics error:") *>
                    stderrln(s"${reason.file.getOrElse("<stdin>")}, ${reason.location}:") *>
                    stderrln(reason.exp)
        io *> stderrln()

    def state: IO[TestCliRuntimeState] = ref.get
}

object TestCliRuntime {
    def create(
        inputs: Map[String, String] = Map.empty,
        pathReads: Map[os.Path, Either[Throwable, String]] = Map.empty
    ): IO[TestCliRuntime] =
        Ref.of[IO, TestCliRuntimeState](TestCliRuntimeState(inputs = inputs, pathReads = pathReads))
            .map(TestCliRuntime(_))
}
