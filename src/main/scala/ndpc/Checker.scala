package ndpc

import ndpc.Parser._
import ndpc.expr.Rule._
import ndpc.expr.Formula._

import scala.io.Source
import scala.util.Try
import parsley.{Success, Failure}
import parsley.Result

case class CheckedProof(main: PfScope[LF_])

extension [A](xs: List[A])
    def remove(x: A): List[A] = xs match {
        case `x` :: tail  => tail
        case head :: tail => head :: tail.remove(x)
        case _            => Nil
    }

object Checker {
    // just controller exceptions that can be pattern matched
    private class ParserException(val reason: String) extends Exception
    private object ParserException:
        def unapply(e: ParserException): Option[String] = Some(e.reason)
    private class CheckException(val reason: String) extends Exception
    private object CheckException:
        def unapply(e: CheckException): Option[String] = Some(e.reason)

    private sealed trait CheckError
    private case class IOError(reason: String) extends CheckError
    private case class SyntaxError(reason: String) extends CheckError
    private case class SemanticsError(reason: String) extends CheckError

    def check(inputs: List[String], toJson: Boolean): Int =
        val errors = pfFromSource(inputs).filter {
            (out: Result[CheckError, CheckedProof]) =>
                out match {
                    case Success(_) => false
                    case _          => true
                }
        }
        if toJson then
            println(buildErrorJson(errors.asInstanceOf[List[CheckError]]))
        else println(buildErrorHuman(errors.asInstanceOf[List[CheckError]]))
        errors.length

    private def buildErrorHuman(errors: List[CheckError]): String = ???

    private def buildErrorJson(errors: List[CheckError]): String = ???

    // I really want consistent error handling here so I went for java exceptions,
    // which is well captured by scala.util.Try
    private def pfFromSource(
        inputs: List[String]
    ): List[Result[CheckError, CheckedProof]] =
        inputs.map { (input: String) =>
            Try(input)
                .map { (i: String) =>
                    val src = i match {
                        case "-"  => Source.stdin
                        case file => Source.fromFile(file)
                    }
                    src.mkString
                }
                .map { (contents: String) =>
                    parse(contents) match {
                        case Success(ast) => ast
                        case Failure(reason) =>
                            throw new ParserException(reason.toString())
                    }
                }
                .map { (upf: UncheckedProof) =>
                    checkOne(upf) match {
                        case Success(pf) => pf
                        case Failure(reason) =>
                            throw new CheckException(reason.toString())
                    }
                } match {
                case scala.util.Success(pf) => Success(pf)
                case scala.util.Failure(exception) => {
                    exception match {
                        case ParserException(reason) =>
                            Failure(SyntaxError(reason))
                        case CheckException(reason) =>
                            Failure(SemanticsError(reason))
                        case throwable @ _ =>
                            Failure(IOError(throwable.toString()))
                    }
                }
            }
        }

    private def checkOne(upf: UncheckedProof): Result[String, CheckedProof] =
        ???

    private def tryVerify(
        input: Line[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        env: Set[String]
    ): Result[String, Line[_]] = input match {
        case nonpf @ (Comment(_) | Empty()) => Success(nonpf)
        // format: off
        case it @ Pf[Int](concl, rule, _) => rule match {
        // All the introductions
        case AndIntro(left, right) => 
            // concl = left ^ right
            tryVerifyAndIntro(it, lineNr, lines, concl, left, right)
        case ImpliesIntro(ass, res) =>
            // concl = ass -> res
            tryVerifyImpliesIntro(it, lineNr, lines, concl, ass, res)
        case OrIntro(either) =>
            // concl = either / x OR concl = x / either
            tryVerifyOrIntro(it, lineNr, lines, concl, either)
        case NotIntro(orig, bottom) =>
            // concl = ~orig, bottom = F
            tryVerifyNotIntro(it, lineNr, lines, concl, orig, bottom)
        case DoubleNegIntro(orig) =>
            // concl = ~~orig
            tryVerifyDoubleNegIntro(it, lineNr, lines, concl, orig)
        case FalsityIntro(orig, negated) =>
            // concl = F, negated = ~orig
            tryVerifyFalsityIntro(it, lineNr, lines, concl, orig, negated)
        case TruthIntro() =>
            // concl = T
            if concl == Truth() then
                Success(it)
            else
                Failure(s"""
                        |line $lineNr:
                        |   $rule expects \"conclusion\" ($concl) to be T
                        """.stripMargin)
        case EquivIntro(leftImp, rightImp) =>
            // leftImp = l -> r, rightImp = r -> l, concl = l <-> r
            tryVerifyEquivIntro(it, lineNr, lines, concl, leftImp, rightImp)
        case ExistsIntro(orig) =>
            // concl = exists x f, where f = orig[x/?], f no free vars
            // and x should not be defined previously
            tryVerifyExistsIntro(it, lineNr, lines, concl, orig, env)
        case ForallIntro(const, conclForall) =>
            // concl = forall sc f, where f = conclForall[sc/?], f no free vars
            // and f free of sk
            // and const = sk, a Var
            //  ..._but_ due to parser constraints, we should expect an 0-arity predAp
            // and x should not be defined previously
            tryVerifyForallIntro(it, lineNr, lines, concl, const, conclForall, env)

        // All the eliminations
        case AndElim(orig) =>
            tryVerifyAndElim(it, lineNr, lines, concl, orig)
        case ImpliesElim(ass, imp) =>
            tryVerifyImpliesElim(it, lineNr, lines, concl, ass, imp)
        case OrElim(ls, le, rs, re) =>
            tryVerifyOrElim(it, lineNr, lines, concl, ls, le, rs, re)
        case NotElim(orig, negated) => ???
        case DoubleNegElim(orig) => ???
        case FalsityElim(bottom) => ???
        case EquivElim(equiv, either) => ???
        case ExistsElim(ass, concl) => ???
        case ForallElim(orig) => ???
        case ForallImpElim(ass, imp) => ???

        // The special ones

        case _ => ???
        }
        // format: on
    }

    private def outOfBound(currLine: Int, rule: Rule[_]) =
        Failure(s"""
                |line $currLine:
                |   $rule specified line numbers that's out of bound
                """.stripMargin)

    private def inBound(it: Int, current: Int) = it < current && it > 0

    private def tryVerifyAndIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        left: Int,
        right: Int
    ): Result[String, Line[LF_]] =
        if inBound(left, lineNr) && inBound(right, lineNr) then
            (lines(left - 1), lines(right - 1)) match {
                case (Pf(l, _, _), Pf(r, _, _)) if (concl == And(l, r)) =>
                    Success(
                      input.copy(rule = AndIntro(l, r))
                    )
                case (l, r) =>
                    Failure(s"""
                            |line $lineNr:
                            |   rule ${input.rule} expects "lhs ^ rhs" to be equal to "conclusion"
                            |   in particular, "($l) ^ ($r)" to be equal to $concl
                            |   but it's not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyImpliesIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        imp: Int,
        res: Int
    ): Result[String, Line[LF_]] =
        if inBound(imp, lineNr) && inBound(res, lineNr) then
            (lines(imp - 1), lines(res - 1)) match {
                case (Pf(i, _, _), Pf(r, _, _)) if (concl == Implies(i, r)) =>
                    Success(
                      input.copy(rule = ImpliesIntro(i, r))
                    )
                case (l, r) =>
                    Failure(s"""
                            |line $lineNr:
                            |   rule ${input.rule} expects "lhs -> rhs" to be equal to "conclusion"
                            |   in particular, "($l) -> ($r)" to be equal to $concl
                            |   but it's not satisfied
                        """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyOrIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        either: Int
    ): Result[String, Line[LF_]] =
        if inBound(either, lineNr) then
            (lines(either - 1), concl) match {
                case (Pf(lf, _, _), Or(l, r)) if lf == l || lf == r =>
                    Success(input.copy(rule = OrIntro(lf)))
                case (e, concl) =>
                    Failure(s"""
                            |line $lineNr:
                            |   rule ${input.rule} expects "x / y" to be equal to "conclusion", where either side is line $either
                            |   in particular, $concl be of form "x / ($either)" or "($either) / x"
                            |   but it's not satisfied
                        """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyNotIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        orig: Int,
        bottom: Int
    ): Result[String, Line[LF_]] =
        if inBound(orig, lineNr) && inBound(bottom, lineNr) then
            (lines(orig - 1), lines(bottom - 1)) match {
                case (Pf(o, _, _), Pf(b, _, _))
                    if (b == Falsity() && concl == Not(o)) =>
                    Success(
                      input.copy(rule = NotIntro(o, b))
                    )
                case (o, b) =>
                    Failure(s"""
                            |line $lineNr:
                            |   rule ${input.rule} expects "conclusion" to be equal to ~(original) and "bottom" to be F
                            |   in particular, $concl to be equal to ~($o), and $b to be F
                            |   but it's not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyDoubleNegIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        orig: Int
    ): Result[String, Line[LF_]] =
        if inBound(orig, lineNr) then
            lines(orig - 1) match {
                case Pf(o, _, _) if concl == Not(Not(o)) =>
                    Success(input.copy(rule = DoubleNegIntro(o)))
                case o =>
                    Failure(s"""
                            |line $lineNr:
                            |   rule ${input.rule} expects "conclusion" to be equal to ~~(original)
                            |   in particular, $concl to be equal to ~~($o)
                            |   but it's not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyFalsityIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        orig: Int,
        negated: Int
    ): Result[String, Line[LF_]] =
        if inBound(orig, lineNr) && inBound(negated, lineNr) then
            (lines(orig - 1), lines(negated - 1)) match {
                case (Pf(o, _, _), Pf(n, _, _))
                    // we don't allow orig and negated to be reversed, same for the others
                    if (n == Not(o) && concl == Falsity()) =>
                    Success(
                      input.copy(rule = FalsityIntro(o, n))
                    )
                case (o, n) =>
                    Failure(s"""
                            |line $lineNr:
                            |   rule ${input.rule} expects "conclusion" to be F and that ~(original) to be equal to "negated"
                            |   in particular, $concl to be F, and ~($o) to be equal to $n
                            |   but it's not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyEquivIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        leftImp: Int,
        rightImp: Int
    ): Result[String, Line[LF_]] =
        if inBound(leftImp, lineNr) && inBound(rightImp, lineNr) then
            (lines(leftImp - 1), lines(rightImp - 1)) match {
                case (Pf(Implies(ll, lr), _, _), Pf(Implies(rl, rr), _, _))
                    if (ll == rr && lr == rl && concl == Equiv(ll, rr)) =>
                    Success(
                      input.copy(rule = EquivIntro(ll, rr))
                    )
                case (l, r) =>
                    Failure(s"""
                            |line $lineNr:
                            |   rule ${input.rule} expects "conclusion" to have the same lhs and rhs as "left implication" and "right implication"
                            |   in particular, $concl to have the same lhs and rhs as $l and $r
                            |   but it's not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyExistsIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        orig: Int,
        env: Set[String]
    ): Result[String, Line[LF_]] =
        if inBound(orig, lineNr) then
            (lines(orig - 1), concl) match {
                case (Pf(o, _, _), Exists(x, f))
                    if !env(x) &&
                        isSubstitutionOf(o, f, x) &&
                        f.getVars().forall(env(_)) =>
                    Success(input.copy(rule = ExistsIntro(o)))
                case (o, c) =>
                    Failure(s"""
                            |line $lineNr:
                            |   rule ${input.rule} expects "conclusion" to be "original" with variables substituted...
                            |   and that the exist quantifier is a new variable "x"...
                            |   and that the body of the exists formula has no free variables...
                            |   and that the body of the exists formula is free of new variable "x"...
                            |   But with 
                            |       "conclusion" = $c
                            |       "original" = $o
                            |   the relations are not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyForallIntro(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        const: Int,
        conclForall: Int,
        env: Set[String]
    ): Result[String, Line[LF_]] =
        if inBound(const, lineNr) && inBound(conclForall, lineNr) then
            (lines(const - 1), lines(conclForall - 1), concl) match {
                case (
                      Pf(c @ PredAp(Predicate(name, 0), Nil), _, _),
                      Pf(fa, _, _),
                      Forall(x, f)
                    )
                    if x == name &&
                        isSubstitutionOf(fa, f, x) &&
                        !env(x) &&
                        !f.getVars()(x) &&
                        f.getVars().forall(env(_)) =>
                    Success(input.copy(rule = ForallIntro(c, fa)))
                case (c, fa, f) =>
                    Failure(s"""
                            |line $lineNr:
                            |   rule ${input.rule} expects "conclusion" to be "original" with variables substituted...
                            |   and that "const" introduced a variable...
                            |   and that the forall quantifier is a new variable...
                            |   and that the body of the exists formula has no free variables.
                            |   But with 
                            |       "conclusion" = $f
                            |       "original" = $fa
                            |       "const" = $c
                            |   the relations are not satisfied
                        """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def isSubstitutionOf(
        original: LF_,
        substituted: LF_,
        x: String
    ): Boolean =
        original == substituted ||
            original
                .getVars()
                // original[t/x] == substituted?
                .exists(t => original.substitute(t, x) == substituted)

    private def tryVerifyAndElim(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        orig: Int
    ) =
        if inBound(orig, lineNr) then
            (lines(orig - 1)) match {
                case Pf(a @ And(l, r), _, _) if l == concl || r == concl =>
                    Success(input.copy(rule = AndElim(a)))
                case a =>
                    Failure(s"""
                            |line $lineNr:
                            |   rule ${input.rule} expects "conclusion" to be either the lhs or rhs of "and"
                            |   in particular, $concl to be the rhs or lhs of $a
                            |   but it's not satisfied
                    """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyImpliesElim(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        ass: Int,
        imp: Int
    ) =
        if inBound(ass, lineNr) && inBound(ass, lineNr) then
            (lines(ass - 1), lines(imp - 1)) match {
                case (Pf(a, _, _), Pf(i, _, _)) if i == Implies(a, concl) =>
                    Success(input.copy(rule = ImpliesElim(a, i)))
                case (a, i) =>
                    Failure(s"""
                            |line $lineNr:
                            |   rule ${input.rule} expects "conclusion" to be the rhs of "implication" and "assumption" to be its lhs
                            |   in particular, $a -> $concl should be equal to $i
                            |   but it's not satisfied
                    """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyOrElim(
        input: Pf[Int],
        lineNr: Int,
        lines: List[Line[Int]],
        concl: LF_,
        leftStart: Int,
        leftEnd: Int,
        rightStart: Int,
        rightEnd: Int
    ) =
        if inBound(leftStart, lineNr) && inBound(rightStart, lineNr) && inBound(
              leftEnd,
              lineNr
            ) && inBound(rightEnd, lineNr)
        then
            (
              lines(leftStart - 1),
              lines(leftEnd - 1),
              lines(rightStart - 1),
              lines(rightEnd - 1)
            ) match {
                case (_, _, _, _) => ???
            }
        else outOfBound(lineNr, input.rule)
}
