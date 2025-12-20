package ndpc.backend

import ndpc.utils.NdpcError
import parsley.{Result, Success, Failure}
import ndpc.utils._
import scala.util.Try
import ndpc.frontend.checker.pfFromSource
import ndpc.frontend.CheckedProof
import ndpc.frontend.expr.rule._
import ndpc.frontend.parser.{Pf, PfScope, Line}

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

    def fromSource(inputs: Seq[String], opt: A): Seq[Output] =
        pfFromSource(inputs).zip(inputs).map { (pf, dest) =>
            pf match
                case Success(pf)    => Success((outputPath(dest), compile(pf, opt)))
                case f @ Failure(_) => f
        }

    def compile(pf: CheckedProof, opt: A): String

    protected val ext: String

    private def outputPath(orig: String): os.Path =
        os.FilePath(
          orig.replaceAll("\\.[^.]*$", "") + s".$ext"
        ).resolveFrom(os.pwd)

    protected def findOrElims(s: PfScope): (Set[(Int, Int)], Set[(Int, Int)]) = {
        s.body.foldLeft((Set.empty, Set.empty)) {
            case ((left, right), Left(Pf(_, OrElim(_, la, lc, ra, rc), _))) =>
                (left incl (la, lc), right incl (ra, rc))
            case ((left, right), Right(sc: PfScope)) =>
                val (leftSub, rightSub) = findOrElims(sc)
                (left ++ leftSub, right ++ rightSub)
            case ((left, right), _) => (left, right)
        }
    }
}
