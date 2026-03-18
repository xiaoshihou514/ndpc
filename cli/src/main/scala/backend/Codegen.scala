package ndpc.backend

import cats.effect.IO
import cats.syntax.all.*
import ndpc.{CliRuntime, IORuntime}
import ndpc.frontend.CheckedProof
import ndpc.frontend.checker.pfFromSource
import ndpc.frontend.expr.formula.*
import ndpc.frontend.expr.rule.*
import ndpc.frontend.parser.{Pf, PfScope, Line}
import ndpc.utils.NdpcError
import parsley.{Result, Success, Failure}

// Helper function for parenthesis handling
private def paren(f: LFormula => String): (LFormula, LFormula) => String = {
    def precedence(lf: LFormula): Int = lf match {
        case PredAp(_, _)  => 7
        case Truth         => 7
        case Falsity       => 7
        case Not(_)        => 6
        case Eq(_, _)      => 5
        case And(_, _)     => 4
        case Or(_, _)      => 3
        case Equiv(_, _)   => 2
        case Implies(_, _) => 1
        case Forall(_, _)  => 0
        case Exists(_, _)  => 0
    }

    { (parent, child) =>
        if precedence(parent) < precedence(child) then f(child)
        else s"(${f(child)})"
    }
}

trait codegen[A] {
    type Output = Result[NdpcError, (os.Path, String)]

    def generate(inputs: Seq[String], opt: A, runtime: CliRuntime = IORuntime): IO[Int] =
        fromSource(inputs, opt, runtime).flatMap { results =>
            val errors = results.collect { case f @ Failure(_) => f }
            val successes = results.collect { case Success(value) => value }

            val printErrors =
                if errors.nonEmpty then runtime.printErrorHuman(errors)
                else IO.unit

            printErrors *> successes.toList.foldLeftM(errors.length) {
                case (code, (dest, result)) =>
                    runtime.writeText(dest, result).attempt.flatMap {
                        case Right(_) => IO.pure(code)
                        case Left(exception) =>
                            runtime.error(s"Can't write to $dest: $exception").as(code + 1)
                    }
            }
        }

    def fromSource(inputs: Seq[String], opt: A, runtime: CliRuntime = IORuntime): IO[Seq[Output]] =
        pfFromSource(inputs, runtime).flatMap {
            _.zip(inputs).toList.traverse { (pf, dest) =>
                pf match
                    case Success(pf) =>
                        compile(pf, opt, runtime).map(result => Success((outputPath(dest), result)))
                    case f @ Failure(_) => IO.pure(f)
            }
        }

    def compile(pf: CheckedProof, opt: A, runtime: CliRuntime): IO[String]

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
