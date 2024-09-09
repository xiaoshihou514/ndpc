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
        input: Line,
        lineNr: Int
    )(using
        lines: List[Line],
        main: PfScope,
        env: Set[String],
        knowledge: Set[Line],
        findParent: Map[Line, PfScope]
    ): Result[String, Option[String]] = {
        given inputContext: Line = input
        given lineNrContext: Int = lineNr
        input match {
            case nonpf @ (Comment(_) | Empty()) => Success(None)
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
                            Success(None)
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
                        tryVerifyNotElim(orig, negated)
                    case DoubleNegElim(orig) => 
                        // ~~concl = orig
                        tryVerifyDoubleNegElim(orig)
                    case FalsityElim(bottom) => 
                        // bottom = F
                        tryVerifyFalsityElim(bottom)
                    case EquivElim(equiv, either) => 
                        // equiv = l <-> r, either = l OR either = r
                        tryVerifyEquivElim(equiv, either)
                    case ExistsElim(exists, ass, assConcl) => 
                        // exists = exists x. ass[c/x], c new var
                        // concl = assConcl
                        tryVerifyExistsElim(exists, ass, assConcl)
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
        Failure(
          s"$rule specified line numbers that's out of bound or trapped in boxes"
        )

    private def inBound(it: Int, current: Int) = it < current && it > 0

    private def verifyArgs(args: List[Int])(using
        lineNr: Int,
        lines: List[Line],
        knowledgeBase: Set[Line]
    ) = args.forall(x => inBound(x, lineNr) && knowledgeBase(lines(x - 1)))

    private def lmap(lineNumber: Int)(using lines: List[Line]) =
        lines(lineNumber - 1)

    private def tryVerifyAndIntro(left: Int, right: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ): Result[String, Option[String]] =
        if verifyArgs(List(left, right)) then
            (lmap(left), lmap(right)) match {
                case (Pf(l, _, _), Pf(r, _, _)) if (concl == And(l, r)) =>
                    Success(None)
                case (l, r) =>
                    Failure(s"""
                            |rule ${input.rule} expects "lhs ^ rhs" to be equal to "conclusion"
                            |in particular, "($l) ^ ($r)" to be equal to $concl
                            |but it's not satisfied
                            """.stripMargin)
            }
        else outOfBound(lineNr, input.rule)

    private def tryVerifyImpliesIntro(ass: Int, res: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line],
        parentLookup: Map[Line, PfScope]
    ): Result[String, Option[String]] =
        if verifyArgs(List(ass, res)) then
            (lmap(ass), lmap(res)) match {
                // whether it's the begining of a box is checked beforehand
                // so the rule being an Ass and ass and res being in the same box is sufficient
                case (al @ Pf(a, Ass(), _), rl @ Pf(r, _, _))
                    if (concl == Implies(a, r) &&
                        parentLookup(al) == parentLookup(rl)) =>
                    Success(None)
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
        concl: LFormula,
        knowledge: Set[Line]
    ): Result[String, Option[String]] =
        if inBound(either, lineNr) then
            (lmap(either), concl) match {
                case (Pf(lf, _, _), Or(l, r)) if lf == l || lf == r =>
                    Success(None)
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
        concl: LFormula,
        knowledge: Set[Line]
    ): Result[String, Option[String]] =
        if verifyArgs(List(orig, bottom)) then
            (lmap(orig), lmap(bottom)) match {
                case (Pf(o, _, _), Pf(Falsity(), _, _)) if (concl == Not(o)) =>
                    Success(None)
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
        concl: LFormula,
        knowledge: Set[Line]
    ): Result[String, Option[String]] =
        if inBound(orig, lineNr) then
            lmap(orig) match {
                case Pf(o, _, _) if concl == Not(Not(o)) =>
                    Success(None)
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
        concl: LFormula,
        knowledge: Set[Line]
    ): Result[String, Option[String]] =
        if verifyArgs(List(orig, negated)) then
            (lmap(orig), lmap(negated)) match {
                case (Pf(o, _, _), Pf(n, _, _))
                    // we don't allow orig and negated to be reversed, same for the others
                    if (n == Not(o) && concl == Falsity()) =>
                    Success(None)
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
        concl: LFormula,
        knowledge: Set[Line]
    ): Result[String, Option[String]] =
        if verifyArgs(List(leftImp, rightImp)) then
            (lmap(leftImp), lmap(rightImp)) match {
                case (Pf(Implies(ll, lr), _, _), Pf(Implies(rl, rr), _, _))
                    if (ll == rr && lr == rl && concl == Equiv(ll, rr)) =>
                    Success(None)
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
        concl: LFormula,
        knowledge: Set[Line]
    ): Result[String, Option[String]] =
        if inBound(orig, lineNr) then
            (lmap(orig), concl) match {
                case (Pf(o, _, _), Exists(x, f))
                    if !env(x) &&
                        isSubstitutionOf(o, f, x) =>
                    // if the above holds then concl won't have any free variables
                    // the proof is left as an exercise
                    Success(None)
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
        concl: LFormula,
        knowledge: Set[Line]
    ): Result[String, Option[String]] =
        if verifyArgs(List(const, conclForall)) then
            (lmap(const), lmap(conclForall), concl) match {
                case (
                      Pf(PredAp(Predicate(c, 0), Nil), _, _),
                      Pf(fa, _, _),
                      Forall(x, f)
                    )
                    if fa.substitute(c, x) == f &&
                        !env(x) &&
                        !(f.getVars() intersect Set(c, x)).isEmpty =>
                    Success(None)
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
        concl: LFormula,
        knowledge: Set[Line]
    ) =
        if inBound(orig, lineNr) then
            (lmap(orig)) match {
                case Pf(And(l, r), _, _) if l == concl || r == concl =>
                    Success(None)
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
        concl: LFormula,
        knowledge: Set[Line]
    ) =
        if verifyArgs(List(ass, imp)) then
            (lmap(ass), lmap(imp)) match {
                case (Pf(a, _, _), Pf(i, _, _)) if i == Implies(a, concl) =>
                    Success(None)
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
        concl: LFormula,
        knowledge: Set[Line],
        pLookup: Map[Line, PfScope]
    ) =
        if verifyArgs(List(or, leftStart, rightStart, leftEnd, rightEnd)) then
            (
              lmap(or),
              lmap(leftStart),
              lmap(leftEnd),
              lmap(rightStart),
              lmap(rightEnd)
            ) match {
                case (
                      or @ Pf(o, _, _),
                      ls @ Pf(lsf, Ass(), _),
                      le @ Pf(lef, _, _),
                      rs @ Pf(rsf, Ass(), _),
                      re @ Pf(ref, _, _)
                    )
                    if pLookup(ls) == pLookup(le) &&
                        pLookup(rs) == pLookup(re) &&
                        lef == ref &&
                        lef == concl &&
                        o == Or(lsf, rsf) =>
                    Success(None)

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

    private def tryVerifyNotElim(orig: Int, negated: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(orig, negated)) then
        (lmap(orig), lmap(negated)) match {
            case (Pf(o, _, _), Pf(n, _, _))
                if n == Not(o) && concl == Falsity() =>
                Success(None)
            case (o, n) =>
                Failure(s"""
                        |rule ${input.rule} expects "~original" equals "negated", and "conclusion" equals F
                        |in particular, ~($orig) to be equal to $negated, and $concl to be equal to F
                """.stripMargin)
        }
    else outOfBound(lineNr, input.rule)

    private def tryVerifyDoubleNegElim(orig: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if inBound(orig, lineNr) then
        lmap(orig) match {
            case Pf(o, _, _) if o == Not(Not(concl)) =>
                Success(None)
            case o =>
                Failure(s"""
                        |rule ${input.rule} expects "~~conclusion" equals "original"
                        |in particular, ~~($concl) to be equal to $o
                """.stripMargin)
        }
    else outOfBound(lineNr, input.rule)

    private def tryVerifyFalsityElim(bottom: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        env: Set[String],
        knowledge: Set[Line]
    ) = if inBound(bottom, lineNr) then
        lmap(bottom) match {
            case Pf(Falsity(), _, _) if concl.getVars().forall(env(_)) =>
                Success(None)
            case b =>
                Failure(s"""
                        |rule ${input.rule} expects "bottom" to be equal to F
                        |in particular, $b to be equal to F
                """.stripMargin)
        }
    else outOfBound(lineNr, input.rule)

    private def tryVerifyEquivElim(equiv: Int, either: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(equiv, either)) then
        (lmap(equiv), lmap(either)) match {
            case (Pf(e, _, _), Pf(ei, _, _))
                if e == Equiv(concl, ei) || e == Equiv(ei, concl) =>
                Success(None)
            case (eq, ei) =>
                Failure(s"""
                        |rule ${input.rule} expects "equiv" to be equal to "conclusion" <-> "either" OR "either" <-> "conclusion"
                        |in particular, $eq to be equal to ($concl) <-> ($ei) or reversed
                """.stripMargin)
        }
    else outOfBound(lineNr, input.rule)

    private def tryVerifyExistsElim(exists: Int, ass: Int, assConcl: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line],
        pLookup: Map[Line, PfScope],
        env: Set[String]
    ) = if verifyArgs(List(exists, ass, assConcl)) then
        (lmap(exists), lmap(ass), lmap(assConcl)) match {
            case (
                  Pf(e @ Exists(x, f), _, _),
                  al @ Pf(a, Ass(), _),
                  cl @ Pf(ac, _, _)
                )
                if concl == ac &&
                    pLookup(al) == pLookup(cl) &&
                    isSubstitutionOf(f, a, x) =>
                (a.getVars() removedAll e.getVars()).toList match {
                    case t :: Nil if env(t) =>
                        Success(None)
                    case _ =>
                        Failure(
                          s"rule ${input.rule} expects implication from exists assumption to be free of assumed variable"
                        )
                }
            case (e, a, ac) =>
                Failure(s"""
                        |rule ${input.rule} expects
                """.stripMargin)
        }
    else outOfBound(lineNr, input.rule)
}
