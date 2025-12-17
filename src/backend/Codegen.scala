package ndpc.backend

import ndpc.utils.NdpcError
import parsley.{Result, Success, Failure}
import ndpc.utils._
import scala.util.Try
import ndpc.frontend.checker.pfFromSource
import ndpc.frontend.CheckedProof

trait codegen[A] {
    type Output = Result[NdpcError, (os.Path, String)]

    def generate(inputs: Seq[String], opt: A): Int = {
        val results = fromSource(inputs, opt)
        val errors = results.collect { case f @ Failure(_) => f }
        val successes = results.flatten

        if !errors.isEmpty then printErrorHuman(errors)

        var code = errors.length
        for ((dest, result) <- successes) do {
            Try(os.write.over(dest, result)) match
                case _: scala.util.Failure[_] => code = code + 1
                case _                        =>
        }
        code
    }

    private def fromSource(inputs: Seq[String], opt: A): Seq[Output] =
        pfFromSource(inputs).zip(inputs).map { (pf, dest) =>
            pf match
                case Success(pf)    => Success((outputPath(dest), compile(pf, opt)))
                case f @ Failure(_) => f
        }

    private def outputPath(orig: String): os.Path =
        os.FilePath(
          orig.replaceAll("\\.[^.]*$", "") + s".$ext"
        ).resolveFrom(os.pwd)

    protected def compile(pf: CheckedProof, opt: A): String
    protected val ext: String
}
