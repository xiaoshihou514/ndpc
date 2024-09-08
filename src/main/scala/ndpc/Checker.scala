package ndpc

import ndpc.Parser._
import ndpc.expr.Rule._
import ndpc.expr.Formula._

import scala.io.Source
import scala.util.Try
import parsley.{Success, Failure}
import scala.collection.mutable.Set
import parsley.Result

case class CheckedProof(main: PfScope)

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
        main: PfScope,
        input: Line,
        lineNr: Int,
        lines: List[Line],
        env: Set[String]
    ): Result[String, Line] = {
        given inputContext: Line = input
        given lineNrContext: Int = lineNr
        given linesContext: List[Line] = lines
        given envContext: Set[String] = env
        input match {
            case nonpf @ (Comment(_) | Empty()) => Success(nonpf)
            // format: off
            case it @ Pf(concl, rule, _) => 
                given conclusion: LFormula = concl
                given thisLine: Pf = it
                rule match {
                    // All the introductions
                    case AndIntro(left, right) => 
                        // concl = left ^ right
                        tryVerifyAndIntro(left, right)
                    case ImpliesIntro(ass, res) =>
                        // concl = ass -> res
                        tryVerifyImpliesIntro(ass, res)
                    case OrIntro(either) =>
                        // concl = either / x OR concl = x / either
                        tryVerifyOrIntro(either)
                    case NotIntro(orig, bottom) =>
                        // concl = ~orig, bottom = F
                        tryVerifyNotIntro(orig, bottom)
                    case DoubleNegIntro(orig) =>
                        // concl = ~~orig
                        tryVerifyDoubleNegIntro(orig)
                    case FalsityIntro(orig, negated) =>
                        // concl = F, negated = ~orig
                        tryVerifyFalsityIntro(orig, negated)
                    case TruthIntro() =>
                        // concl = T
                        if concl == Truth() then
                            Success(it)
                        else
                            Failure(s"""
                                    |$rule expects "conclusion" ($concl) to be T
                                    """.stripMargin)
                    case EquivIntro(leftImp, rightImp) =>
                        // leftImp = l -> r, rightImp = r -> l, concl = l <-> r
                        tryVerifyEquivIntro(leftImp, rightImp)
                    case ExistsIntro(orig) =>
                        // concl = exists x f, where f = orig[x/?], f no free vars
                        // and x should not be defined previously
                        tryVerifyExistsIntro(orig, env)
                    case ForallIntro(const, conclForall) =>
                        // concl = forall sc f, where f = conclForall[sc/?], f no free vars
                        // and f free of sk
                        // and const = sk, a Var
                        //  ..._but_ due to parser constraints, we should expect an 0-arity predAp
                        // and x should not be defined previously
                        tryVerifyForallIntro(const, conclForall, env)

                    // All the eliminations
                    case AndElim(orig) =>
                        // concl = orig ^ x OR concl = x ^ orig
                        tryVerifyAndElim(orig)
                    case ImpliesElim(ass, imp) =>
                        // imp = ass -> concl
                        tryVerifyImpliesElim(ass, imp)
                    case OrElim(or, ls, le, rs, re) =>
                        // (ls, le), (rs, re) start and end of box
                        // or = ls / rs
                        // le = re = concl
                        tryVerifyOrElim(or, ls, le, rs, re, main)
                    case NotElim(orig, negated) => 
                        // ~orig = negated
                        // concl = F
                        ???
                    case DoubleNegElim(orig) => 
                        // ~~concl = orig
                        ???
                    case FalsityElim(bottom) => 
                        // bottom = F
                        ???
                    case EquivElim(equiv, either) => 
                        // equiv = l <-> r, either = l OR either = r
                        ???
                    case ExistsElim(exists, ass, assConcl) => 
                        // exists = exists x. ass[c/x], c new var
                        // concl = assConcl
                        ???
                    case ForallElim(orig) => 
                        // orig = forall x. concl[c/x], c in env
                        ???
                    case ForallImpElim(ass, imp) => 
                        // imp = forall x. f(x) -> g(x)
                        // ass = f(c), concl = g(c), c in env
                        ???

                    // The special ones

                    case _ => ???
                }
            // format: on
        }
    }

    private def outOfBound(currLine: Int, rule: Rule) =
        Failure(s"""
                |line $currLine:
                |   $rule specified line numbers that's out of bound
                """.stripMargin)

    private def inBound(it: Int, current: Int) = it < current && it > 0

    private def tryVerifyAndIntro(left: Int, right: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula
    ): Result[String, Line] =
        if inBound(left, lineNr) && inBound(right, lineNr) then
            (lines(left - 1), lines(right - 1)) match {
                case (Pf(l, _, _), Pf(r, _, _)) if (concl == And(l, r)) =>
                    Success(input)
                case (l, r) =>
                    Failure(s"""
                            |rule ${input.rule} expects "lhs ^ rhs" to be equal to "conclusion"
                            |in particular, "($l) ^ ($r)" to be equal to $concl
                            |but it's not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyImpliesIntro(imp: Int, res: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula
    ): Result[String, Line] =
        if inBound(imp, lineNr) && inBound(res, lineNr) then
            (lines(imp - 1), lines(res - 1)) match {
                case (Pf(i, _, _), Pf(r, _, _)) if (concl == Implies(i, r)) =>
                    Success(input)
                case (l, r) =>
                    Failure(s"""
                            |rule ${input.rule} expects "lhs -> rhs" to be equal to "conclusion"
                            |in particular, "($l) -> ($r)" to be equal to $concl
                            |but it's not satisfied
                        """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyOrIntro(either: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula
    ): Result[String, Line] =
        if inBound(either, lineNr) then
            (lines(either - 1), concl) match {
                case (Pf(lf, _, _), Or(l, r)) if lf == l || lf == r =>
                    Success(input)
                case (e, concl) =>
                    Failure(s"""
                            |rule ${input.rule} expects "x / y" to be equal to "conclusion", where either side is line $either
                            |in particular, $concl be of form "x / ($either)" or "($either) / x"
                            |but it's not satisfied
                        """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyNotIntro(orig: Int, bottom: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula
    ): Result[String, Line] =
        if inBound(orig, lineNr) && inBound(bottom, lineNr) then
            (lines(orig - 1), lines(bottom - 1)) match {
                case (Pf(o, _, _), Pf(b, _, _))
                    if (b == Falsity() && concl == Not(o)) =>
                    Success(input)
                case (o, b) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be equal to ~(original) and "bottom" to be F
                            |in particular, $concl to be equal to ~($o), and $b to be F
                            |but it's not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyDoubleNegIntro(orig: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula
    ): Result[String, Line] =
        if inBound(orig, lineNr) then
            lines(orig - 1) match {
                case Pf(o, _, _) if concl == Not(Not(o)) =>
                    Success(input)
                case o =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be equal to ~~(original)
                            |in particular, $concl to be equal to ~~($o)
                            |but it's not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyFalsityIntro(orig: Int, negated: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula
    ): Result[String, Line] =
        if inBound(orig, lineNr) && inBound(negated, lineNr) then
            (lines(orig - 1), lines(negated - 1)) match {
                case (Pf(o, _, _), Pf(n, _, _))
                    // we don't allow orig and negated to be reversed, same for the others
                    if (n == Not(o) && concl == Falsity()) =>
                    Success(input)
                case (o, n) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be F and that ~(original) to be equal to "negated"
                            |in particular, $concl to be F, and ~($o) to be equal to $n
                            |but it's not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyEquivIntro(leftImp: Int, rightImp: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula
    ): Result[String, Line] =
        if inBound(leftImp, lineNr) && inBound(rightImp, lineNr) then
            (lines(leftImp - 1), lines(rightImp - 1)) match {
                case (Pf(Implies(ll, lr), _, _), Pf(Implies(rl, rr), _, _))
                    if (ll == rr && lr == rl && concl == Equiv(ll, rr)) =>
                    Success(input)
                case (l, r) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to have the same lhs and rhs as "left implication" and "right implication"
                            |in particular, $concl to have the same lhs and rhs as $l and $r
                            |but it's not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyExistsIntro(orig: Int, env: Set[String])(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula
    ): Result[String, Line] =
        if inBound(orig, lineNr) then
            (lines(orig - 1), concl) match {
                case (Pf(o, _, _), Exists(x, f))
                    if !env(x) &&
                        isSubstitutionOf(o, f, x) &&
                        f.getVars().forall(env(_)) =>
                    Success(input)
                case (o, c) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be "original" with variables substituted...
                            |and that the exist quantifier is a new variable "x"...
                            |and that the body of the exists formula has no free variables...
                            |and that the body of the exists formula is free of new variable "x"...
                            |But with 
                            |    "conclusion" = $c
                            |    "original" = $o
                            |the relations are not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyForallIntro(
        const: Int,
        conclForall: Int,
        env: Set[String]
    )(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula
    ): Result[String, Line] =
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
                    Success(input)
                case (c, fa, f) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be "original" with variables substituted...
                            |and that "const" introduced a variable...
                            |and that the forall quantifier is a new variable...
                            |and that the body of the exists formula has no free variables.
                            |But with 
                            |    "conclusion" = $f
                            |    "original" = $fa
                            |    "const" = $c
                            |the relations are not satisfied
                        """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def isSubstitutionOf(
        original: LFormula,
        substituted: LFormula,
        x: String
    ): Boolean =
        original == substituted ||
            original
                .getVars()
                // original[t/x] == substituted?
                .exists(t => original.substitute(t, x) == substituted)

    private def tryVerifyAndElim(orig: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula
    ) =
        if inBound(orig, lineNr) then
            (lines(orig - 1)) match {
                case Pf(a @ And(l, r), _, _) if l == concl || r == concl =>
                    Success(input)
                case a =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be either the lhs or rhs of "and"
                            |in particular, $concl to be the rhs or lhs of $a
                            |but it's not satisfied
                    """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyImpliesElim(ass: Int, imp: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula
    ) =
        if inBound(ass, lineNr) && inBound(ass, lineNr) then
            (lines(ass - 1), lines(imp - 1)) match {
                case (Pf(a, _, _), Pf(i, _, _)) if i == Implies(a, concl) =>
                    Success(input)
                case (a, i) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be the rhs of "implication" and "assumption" to be its lhs
                            |in particular, $a -> $concl should be equal to $i
                            |but it's not satisfied
                    """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyOrElim(
        or: Int,
        leftStart: Int,
        leftEnd: Int,
        rightStart: Int,
        rightEnd: Int,
        main: PfScope
    )(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula
    ) =
        if inBound(or, lineNr) &&
            inBound(leftStart, lineNr) && inBound(rightStart, lineNr) &&
            inBound(leftEnd, lineNr) && inBound(rightEnd, lineNr)
        then
            (
              lines(or - 1),
              lines(leftStart - 1),
              lines(leftEnd - 1),
              lines(rightStart - 1),
              lines(rightEnd - 1)
            ) match {
                case (
                      or @ Pf(o, _, _),
                      ls @ Pf(lsf, _, _),
                      le @ Pf(lef, _, _),
                      rs @ Pf(rsf, _, _),
                      re @ Pf(ref, _, _)
                    )
                    if main.isStartAndEndOfScope(ls, le) &&
                        main.isStartAndEndOfScope(rs, re) &&
                        lef == ref &&
                        lef == concl &&
                        o == Or(lsf, rsf) =>
                    Success(input)

                case (or, ls, le, rs, re) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to hold when either "lhs" or "rhs" is true
                            |"lhs start", "lhs end" should be the start and end of a box...
                            |"rhs start", "rhs end" should be the start and end of a box...
                            |"lhs end" and "rhs end" should both be equal to "conclustion"...
                            |"lhs start" / "rhs start" should be equal to the "or statement".
                            |But with
                            |    "or statement" = $or
                            |    "lhs start" = $ls
                            |    "lhs end" = $le
                            |    "rhs start" = $rs
                            |    "rhs end" = $re
                            |    "conclustion" = $concl
                            |the conditions were not satisfied.
                    """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)
}
