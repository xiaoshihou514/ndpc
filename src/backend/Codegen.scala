package ndpc.backend

import ndpc.utils.NdpcError
import parsley.{Result, Success, Failure}
import ndpc.utils._
import scala.util.Try

trait codegen[A] {
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
    protected def fromSource(inputs: Seq[String], opt: A): Seq[Result[NdpcError, (os.Path, String)]]
}
