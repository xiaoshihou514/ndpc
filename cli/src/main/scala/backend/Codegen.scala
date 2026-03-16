package ndpc.backend

import cats.effect.IO
import cats.syntax.all._
import ndpc.cliRuntime
import ndpc.frontend.CheckedProof
import ndpc.frontend.checker.pfFromSource
import ndpc.frontend.expr.rule._
import ndpc.frontend.parser.{Pf, PfScope, Line}
import ndpc.utils.NdpcError
import parsley.{Result, Success, Failure}

trait codegen[A] {
    type Output = Result[NdpcError, (os.Path, String)]

    def generate(inputs: Seq[String], opt: A): IO[Int] =
        fromSource(inputs, opt).flatMap { results =>
            val errors = results.collect { case f @ Failure(_) => f }
            val successes = results.flatten

            val printErrors =
                if errors.nonEmpty then cliRuntime.printErrorHuman(errors)
                else IO.unit

            printErrors *> successes.toList.foldLeftM(errors.length) { case (code, (dest, result)) =>
                cliRuntime.writeText(dest, result).attempt.flatMap {
                    case Right(_) => IO.pure(code)
                    case Left(exception) =>
                        cliRuntime.error(s"Can't write to $dest: $exception").as(code + 1)
                }
            }
        }

    def fromSource(inputs: Seq[String], opt: A): IO[Seq[Output]] =
        pfFromSource(inputs).flatMap {
            _.zip(inputs).toList.traverse { (pf, dest) =>
                pf match
                    case Success(pf) => compile(pf, opt).map(result => Success((outputPath(dest), result)))
                    case f @ Failure(_) => IO.pure(f)
            }
        }

    def compile(pf: CheckedProof, opt: A): IO[String]

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
