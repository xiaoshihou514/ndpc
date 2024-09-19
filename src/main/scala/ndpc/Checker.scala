package ndpc

import ndpc.Parser._
import ndpc.expr.Rule._
import ndpc.expr.Formula._
import ndpc.Utils._

import scala.io.Source
import scala.util.Try
import scala.collection.mutable.Set
import parsley.{Success, Failure}
import parsley.Result
import scala.util.boundary

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
        if errors != Nil then
            if toJson then buildErrorJson(errors.asInstanceOf[List[CheckError]])
            else buildErrorHuman(errors.asInstanceOf[List[CheckError]])
        else ok("All proofs are valid!")
        errors.length

    private def buildErrorHuman(errors: List[CheckError]) =
        error(errors.mkString)

    private def buildErrorJson(errors: List[CheckError]) =
        println("buildErrorJson")

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

    private def isPremise(line: Line | PfScope) =
        line match
            case Pf(_, Given() | Premise(), _) => true
            case _                             => false

    private def isComment(line: Line | PfScope) =
        line match {
            case Empty() | Comment(_) => true
            case _                    => false
        }

    private def checkOne(upf: UncheckedProof): Result[String, CheckedProof] = {
        val pfs = upf.main.body.dropWhile(x => isPremise(x) || isComment(x))
        if pfs.exists(supposedlyPf =>
                supposedlyPf match {
                    case l @ Pf(_, _, _) => isPremise(l)
                    case ps @ PfScope(_) => ps.exists(isPremise(_))
                    case _               => false
                }
            )
        then
            // TODO: put line number here
            Failure(
              "Did not expect \"Assume\" and \"Premise\" to be used in places other than the start of proof"
            )
        else
            given lines: List[Line] = upf.lines
            given main: PfScope = upf.main
            given Set[String] = Set()
            given Set[Line] = Set()
            given Set[(Line, Line)] = Set()

            tryVerify(main, 1) match {
                case f @ Failure(_) => f
                case _              => Success(CheckedProof(main))
            }
    }

    private def tryVerify(
        input: PfScope,
        lineNr: Int
    )(using
        lines: List[Line],
        main: PfScope,
        env: Set[String],
        knowledge: Set[Line],
        boxConcls: Set[(Line, Line)]
    ): Result[String, Int] = {
        // it's readable, but pretty ugly IMO
        boundary {
            // verify head
            input.body
                .dropWhile(x => isComment(x))
                .head match {
                case Pf(_, Ass() | ForallIConst(), _) => // pass
                case Pf(_, Given() | Premise(), _)    => // pass
                case pf @ Pf(_, _, _) =>
                    boundary.break(
                      Failure(
                        s"Line $pf is the first line of a box, but is not an assumption/given/premise/forall I const"
                      )
                    )
                case _ => // pass
            }

            // build up state
            val localKnowledge: Set[Line] = Set()

            var offset = 0
            for (line <- input.body) do {
                line match {
                    case p @ PfScope(_) =>
                        given Set[Line] = knowledge addAll localKnowledge
                        tryVerify(p, lineNr + offset) match {
                            case f @ Failure(_)   => boundary.break(f)
                            case Success(elapsed) => offset = offset + elapsed
                        }
                    case line =>
                        tryVerifyLine(
                          line.asInstanceOf[Line],
                          lineNr + offset,
                          knowledge union localKnowledge
                        ) match {
                            case Failure(reason) =>
                                boundary.break(
                                  Failure(s"line ${lineNr + offset}:$reason")
                                )
                            case Success(vars) =>
                                env addAll vars
                                if line.isInstanceOf[Pf] then
                                    localKnowledge add line.asInstanceOf[Line]
                        }
                        offset = offset + 1
                }
            }

            // we should leave with nothing but the conclusion
            // we may not need the vars also, but let's omit that for brecity
            boxConcls add (
              input.body
                  .dropWhile(isComment(_))
                  .head
                  .asInstanceOf[Line],
              input.body.reverse
                  .dropWhile(isComment(_))
                  .head
                  .asInstanceOf[Line]
            )
            Success(offset)
        }
    }

    private def tryVerifyLine(
        input: Line,
        lineNr: Int,
        knowledge: Set[Line]
    )(using
        lines: List[Line],
        main: PfScope,
        env: Set[String],
        boxConcls: Set[(Line, Line)]
    ): Result[String, List[String]] = {
        given Set[Line] = knowledge
        given Line = input
        given Int = lineNr
        input match {
            case nonpf @ (Comment(_) | Empty()) => Success(Nil)
            // format: off
            case it @ Pf(concl, rule, _) => 
                given conclusion: LFormula = concl
                given thisLine: Pf = it
                rule match {
                    // All the introductions
                    case AndIntro(left, right) => 
                        tryVerifyAndIntro(left, right)
                    case ImpliesIntro(ass, res) =>
                        given Set[Line] = knowledge addAll boxConcls.map(_.toList).flatten
                        tryVerifyImpliesIntro(ass, res)
                    case OrIntro(either) =>
                        tryVerifyOrIntro(either)
                    case NotIntro(orig, bottom) =>
                        given Set[Line] = knowledge addAll boxConcls.map(_.toList).flatten
                        tryVerifyNotIntro(orig, bottom)
                    case DoubleNegIntro(orig) =>
                        tryVerifyDoubleNegIntro(orig)
                    case FalsityIntro(negated, orig) =>
                        tryVerifyFalsityIntro(orig, negated)
                    case TruthIntro() =>
                        // concl = T
                        if concl == Truth() then
                            Success(Nil)
                        else
                            Failure(s"""
                                    |$rule expects "conclusion" ($concl) to be T
                                    |""".stripMargin)
                    case EquivIntro(leftImp, rightImp) =>
                        tryVerifyEquivIntro(leftImp, rightImp)
                    case ExistsIntro(orig) =>
                        tryVerifyExistsIntro(orig)
                    case ForallIntro(const, conclForall) =>
                        given Set[Line] = knowledge addAll boxConcls.map(_.toList).flatten
                        tryVerifyForallIntro(const, conclForall, env)

                    // All the eliminations
                    case AndElim(orig) =>
                        tryVerifyAndElim(orig)
                    case ImpliesElim(imp, ass) =>
                        tryVerifyImpliesElim(imp, ass)
                    case OrElim(or, leftAss, leftConcl, rightAss, rightConcl) =>
                        given Set[Line] = knowledge addAll boxConcls.map(_.toList).flatten
                        tryVerifyOrElim(or, leftAss, leftConcl, rightAss, rightConcl)
                    case NotElim(negated, orig) => 
                        tryVerifyNotElim(negated, orig)
                    case DoubleNegElim(orig) => 
                        tryVerifyDoubleNegElim(orig)
                    case FalsityElim(bottom) => 
                        tryVerifyFalsityElim(bottom)
                    case EquivElim(equiv, either) => 
                        tryVerifyEquivElim(equiv, either)
                    case ExistsElim(exists, ass, conclExists) => 
                        given Set[Line] = knowledge addAll boxConcls.map(_.toList).flatten
                        tryVerifyExistsElim(exists, ass, conclExists)
                    case ForallElim(orig) => 
                        tryVerifyForallElim(orig)
                    case ForallImpElim(imp, ass) => 
                        tryVerifyForallImpElim(imp, ass)

                    // The special ones
                    case LEM() =>
                        tryVerifyLEM()
                    case MT(imp, negated) =>
                        tryVerifyMT(imp, negated)
                    case PC(orig, bottom) =>
                        given Set[Line] = knowledge addAll boxConcls.map(_.toList).flatten
                        tryVerifyPC(orig, bottom)
                    case Refl() =>
                        tryVerifyRefl()
                    case EqSub(orig, eq) =>
                        tryVerifyEqSub(orig, eq)
                    case Sym(eq) =>
                        tryVerifySym(eq)
                    case ForallIConst() =>
                        tryVerifyForallIConst()
                    case Given() | Premise() =>
                        Success(concl.getVars().toList)
                    case Ass() =>
                        Success(concl.getVars().toList)
                    case Tick(orig) =>
                        tryVerifyTick(orig)
                }
            // format: on
        }
    }

    private def outOfBound(currLine: Int)(using input: Pf) =
        Failure(
          s"${input.rule} specified line numbers that's out of bound or trapped in boxes"
        )

    private def inBound(it: Int, current: Int) = it < current && it > 0

    private def verifyArgs(args: List[Int])(using
        lineNr: Int,
        lines: List[Line],
        knowledgeBase: Set[Line]
    ) = args.forall(x => inBound(x, lineNr) && knowledgeBase(lines(x - 1)))

    private def lmap(lineNumber: Int)(using lines: List[Line]) =
        lines(lineNumber - 1)

    private def tryVerifyAndIntro(leftLine: Int, rightLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) =
        if verifyArgs(List(leftLine, rightLine)) then
            // concl = left ^ right
            (lmap(leftLine), lmap(rightLine)) match {
                case (Pf(left, _, _), Pf(right, _, _))
                    if (concl == And(left, right)) =>
                    Success(Nil)
                case (left, right) =>
                    Failure(s"""
                            |rule ${input.rule} expects "lhs ^ rhs" to be equal to "conclusion"
                            |in particular, "($left) ^ ($right)" to be equal to $concl
                            |but it's not satisfied
                            |""".stripMargin)
            }
        else outOfBound(lineNr)

    private def tryVerifyImpliesIntro(assLine: Int, resLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line],
        boxConcls: Set[(Line, Line)]
    ) =
        if verifyArgs(List(assLine, resLine)) then
            (lmap(assLine), lmap(resLine)) match {
                // concl = ass -> res
                // ass <==> res
                case (a @ Pf(ass, Ass(), _), r @ Pf(res, _, _))
                    if (concl == Implies(ass, res) && boxConcls((a, r))) =>
                    Success(Nil)
                case (ass, res) =>
                    Failure(s"""
                            |rule ${input.rule} expects "assumption -> implication" to be equal to "conclusion"
                            |in particular, "($ass) -> ($res)" to be equal to $concl
                            |but it's not satisfied
                        |""".stripMargin)
            }
        else outOfBound(lineNr)

    private def tryVerifyOrIntro(eitherLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) =
        if verifyArgs(List(eitherLine)) then
            (lmap(eitherLine), concl) match {
                // concl = either / x OR concl = x / either
                case (Pf(either, _, _), Or(left, right))
                    if either == left || either == right =>
                    Success(Nil)
                case (either, _) =>
                    Failure(s"""
                            |rule ${input.rule} expects "x / y" to be equal to "conclusion", where either side is line $either
                            |in particular, $concl be of form "x / ($either)" or "($either) / x"
                            |but it's not satisfied
                        |""".stripMargin)
            }
        else outOfBound(lineNr)

    private def tryVerifyNotIntro(origLine: Int, bottomLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) =
        if verifyArgs(List(origLine, bottomLine)) then
            (lmap(origLine), lmap(bottomLine)) match {
                // concl = ~orig
                // bottom = F
                case (Pf(orig, _, _), Pf(Falsity(), _, _))
                    if (concl == Not(orig)) =>
                    Success(Nil)
                case (orig, bottom) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be equal to ~(original) and "bottom" to be F
                            |in particular, $concl to be equal to ~($orig), and $bottom to be F
                            |but it's not satisfied
                            |""".stripMargin)
            }
        else outOfBound(lineNr)

    private def tryVerifyDoubleNegIntro(origLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) =
        if verifyArgs(List(origLine)) then
            lmap(origLine) match {
                // concl = ~~orig
                case Pf(orig, _, _) if concl == Not(Not(orig)) =>
                    Success(Nil)
                case orig =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be equal to ~~(original)
                            |in particular, $concl to be equal to ~~($orig)
                            |but it's not satisfied
                            |""".stripMargin)
            }
        else outOfBound(lineNr)

    private def tryVerifyFalsityIntro(origLine: Int, negatedLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) =
        if verifyArgs(List(origLine, negatedLine)) then
            (lmap(origLine), lmap(negatedLine)) match {
                // concl = F
                // ~orig = negated
                case (Pf(orig, _, _), Pf(negated, _, _))
                    // we don't allow orig and negated to be reversed, same for the others
                    if (negated == Not(orig) && concl == Falsity()) =>
                    Success(Nil)
                case (orig, negated) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be F and that ~(original) to be equal to "negated"
                            |in particular, $concl to be F, and ~($orig) to be equal to $negated
                            |but it's not satisfied
                            |""".stripMargin)
            }
        else outOfBound(lineNr)

    private def tryVerifyEquivIntro(leftImpLine: Int, rightImpLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) =
        if verifyArgs(List(leftImpLine, rightImpLine)) then
            (lmap(leftImpLine), lmap(rightImpLine)) match {
                // leftImp = x -> y
                // rightImp = y -> x
                // concl = x <-> y
                case (Pf(Implies(ll, lr), _, _), Pf(Implies(rl, rr), _, _))
                    if (ll == rr && lr == rl && concl == Equiv(ll, rr)) =>
                    Success(Nil)
                case (l, r) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to have the same lhs and rhs as "left implication" and "right implication"
                            |in particular, $concl to have the same lhs and rhs as $l and $r
                            |but it's not satisfied
                            |""".stripMargin)
            }
        else outOfBound(lineNr)

    private def tryVerifyExistsIntro(origLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line],
        env: Set[String]
    ) =
        if verifyArgs(List(origLine)) then
            (lmap(origLine), concl) match {
                // concl = exists x. orig[?/x]
                // x free
                case (Pf(orig, _, _), Exists(x, conclF))
                    if !env(x) &&
                        isSubstitutionOf(orig, conclF, x) =>
                    // if the above holds then concl won't have any free variables
                    // the proof is left as an exercise
                    Success(Nil)
                case (orig, _) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be "original" with variables substituted...
                            |and that the exist quantifier is a new variable "x"...
                            |and that the body of the exists formula has no free variables...
                            |and that the body of the exists formula is free of new variable "x"...
                            |But with 
                            |    "conclusion" = $concl
                            |    "original" = $orig
                            |the relations are not satisfied
                            |""".stripMargin)
            }
        else outOfBound(lineNr)

    private def tryVerifyForallIntro(
        constLine: Int,
        conclForallLine: Int,
        env: Set[String]
    )(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) =
        if verifyArgs(List(constLine, conclForallLine)) then
            (lmap(constLine), lmap(conclForallLine), concl) match {
                // const = c
                // concl = forall x. conclF[c/x]
                // x free
                case (
                      Pf(PredAp(Predicate(c, 0), Nil), _, _),
                      Pf(conclF, _, _),
                      Forall(x, f)
                    ) if conclF.substitute(c, x) == f && !env(x) =>
                    Success(Nil)
                case (c, fa, _) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be "original" with variables substituted...
                            |and that "const" introduced a variable...
                            |and that the forall quantifier is a new variable...
                            |and that the body of the exists formula has no free variables.
                            |But with 
                            |    "conclusion" = $concl
                            |    "original" = $fa
                            |    "const" = $c
                            |the relations are not satisfied
                        |""".stripMargin)
            }
        else outOfBound(lineNr)

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

    private def tryVerifyAndElim(origLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) =
        if verifyArgs(List(origLine)) then
            (lmap(origLine)) match {
                // orig = concl ^ x OR orig = x ^ concl
                case Pf(And(left, right), _, _)
                    if left == concl || right == concl =>
                    Success(Nil)
                case and =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be either the lhs or rhs of "and"
                            |in particular, $concl to be the rhs or lhs of $and
                            |but it's not satisfied
                    |""".stripMargin)
            }
        else outOfBound(lineNr)

    private def tryVerifyImpliesElim(impLime: Int, assLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) =
        if verifyArgs(List(assLine, impLime)) then
            (lmap(assLine), lmap(impLime)) match {
                // imp = ass -> concl
                case (Pf(ass, _, _), Pf(imp, _, _))
                    if imp == Implies(ass, concl) =>
                    Success(Nil)
                case (ass, imp) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to be the rhs of "implication" and "assumption" to be its lhs
                            |in particular, $ass -> $concl should be equal to $imp
                            |but it's not satisfied
                    |""".stripMargin)
            }
        else outOfBound(lineNr)

    private def tryVerifyOrElim(
        orLine: Int,
        leftAssLine: Int,
        leftConclLine: Int,
        rightAssLine: Int,
        rightConclLine: Int
    )(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line],
        boxConcls: Set[(Line, Line)],
        main: PfScope
    ) =
        if verifyArgs(
              List(
                orLine,
                leftAssLine,
                rightAssLine,
                leftConclLine,
                rightConclLine
              )
            )
        then
            (
              lmap(orLine),
              lmap(leftAssLine),
              lmap(leftConclLine),
              lmap(rightAssLine),
              lmap(rightConclLine)
            ) match {
                // leftAss <==> leftConcl
                // RightAss <==> RightConcl
                // or = leftAss / rightAss
                // leftConcl = rightConcl = concl
                case (
                      Pf(or, _, _),
                      la @ Pf(leftAss, Ass(), _),
                      lc @ Pf(leftConcl, _, _),
                      ra @ Pf(rightAss, Ass(), _),
                      rc @ Pf(rightConcl, _, _)
                    )
                    if boxConcls((la, lc)) &&
                        boxConcls((ra, rc)) &&
                        leftConcl == rightConcl &&
                        leftConcl == concl &&
                        or == Or(leftAss, rightAss) =>
                    Success(Nil)
                case (or, la, lc, ra, rc) =>
                    Failure(s"""
                            |rule ${input.rule} expects "conclusion" to hold when either "lhs" or "rhs" is true
                            |"lhs start", "lhs end" should be the start and end of a box...
                            |"rhs start", "rhs end" should be the start and end of a box...
                            |"lhs end" and "rhs end" should both be equal to "conclustion"...
                            |"lhs start" / "rhs start" should be equal to the "or statement".
                            |But with
                            |    "or statement" = $or
                            |    "lhs start" = $la
                            |    "lhs end" = $lc
                            |    "rhs start" = $ra
                            |    "rhs end" = $rc
                            |    "conclustion" = $concl
                            |the conditions were not satisfied.
                    |""".stripMargin)
            }
        else outOfBound(lineNr)

    private def tryVerifyNotElim(negatedLine: Int, origLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(origLine, negatedLine)) then
        (lmap(origLine), lmap(negatedLine), concl) match {
            // negated = ~orig
            // concl = F
            case (Pf(orig, _, _), Pf(negated, _, _), Falsity())
                if negated == Not(orig) =>
                Success(Nil)
            case (orig, negated, _) =>
                Failure(s"""
                        |rule ${input.rule} expects "~original" equals "negated", and "conclusion" equals F
                        |in particular, ~($orig) to be equal to $negated, and $concl to be equal to F
                |""".stripMargin)
        }
    else outOfBound(lineNr)

    private def tryVerifyDoubleNegElim(origLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(origLine)) then
        lmap(origLine) match {
            // orig = ~~concl
            case Pf(orig, _, _) if orig == Not(Not(concl)) =>
                Success(Nil)
            case orig =>
                Failure(s"""
                        |rule ${input.rule} expects "~~conclusion" equals "original"
                        |in particular, ~~($concl) to be equal to $orig
                |""".stripMargin)
        }
    else outOfBound(lineNr)

    private def tryVerifyFalsityElim(bottomLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        env: Set[String],
        knowledge: Set[Line]
    ) = if verifyArgs(List(bottomLine)) then
        lmap(bottomLine) match {
            // bottom = F
            // concl bounded
            case Pf(Falsity(), _, _) if concl.getVars().forall(env(_)) =>
                Success(Nil)
            case b =>
                Failure(s"""
                        |rule ${input.rule} expects "bottom" to be equal to F
                        |in particular, $b to be equal to F
                |""".stripMargin)
        }
    else outOfBound(lineNr)

    private def tryVerifyEquivElim(equivLine: Int, eitherLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(equivLine, eitherLine)) then
        (lmap(equivLine), lmap(eitherLine)) match {
            // equiv = either <-> concl OR equiv = concl <-> either
            case (Pf(equiv, _, _), Pf(either, _, _))
                if equiv == Equiv(concl, either) ||
                    equiv == Equiv(either, concl) =>
                Success(Nil)
            case (equiv, either) =>
                Failure(s"""
                        |rule ${input.rule} expects "equiv" to be equal to "conclusion" <-> "either" OR "either" <-> "conclusion"
                        |in particular, $equiv to be equal to ($concl) <-> ($either) or reversed
                |""".stripMargin)
        }
    else outOfBound(lineNr)

    private def tryVerifyExistsElim(
        existsLine: Int,
        assLine: Int,
        conclELine: Int
    )(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line],
        boxConcls: Set[(Line, Line)],
        env: Set[String]
    ) = if verifyArgs(List(existsLine, assLine, conclELine)) then
        (lmap(existsLine), lmap(assLine), lmap(conclELine)) match {
            // ass <==> conclE
            // exists = exists x. ass[?/x]
            // concl = conclE
            // conclE free of ?
            case (
                  Pf(ex @ Exists(x, assE), _, _),
                  al @ Pf(ass, _, _),
                  cl @ Pf(conclE, _, _)
                )
                if concl == conclE &&
                    boxConcls((al, cl)) &&
                    isSubstitutionOf(ass, assE, x) =>
                (ass.getVars() removedAll assE.getVars()).toList match {
                    case t :: Nil if !conclE.getVars()(t) =>
                        Success(Nil)
                    case _ =>
                        Failure(
                          s"rule ${input.rule} expects implication from exists assumption to be free of assumed variable"
                        )
                }
            case (exists, ass, conclE) =>
                // TODO: more helpful error msg
                Failure(s"""
                        |rule ${input.rule} expects reasons to be proofs
                |""".stripMargin)
        }
    else outOfBound(lineNr)

    private def tryVerifyForallElim(origLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line],
        env: Set[String]
    ) = if verifyArgs(List(origLine)) then
        lmap(origLine) match {
            // orig = forall x. concl[?/x]
            // ? bounded
            case Pf(Forall(x, conclF), _, _)
                if isSubstitutionOf(concl, conclF, x) =>
                (concl.getVars() removedAll conclF.getVars()).toList match {
                    case t :: Nil if env(t) =>
                        Success(Nil)
                    case _ =>
                        Failure(
                          s"rule ${input.rule} expects substitution of forall uses bounded variable"
                        )
                }
            case orig =>
                Failure(
                  s"""
                    |rule ${input.rule} expects "original" = forall "x". "conclusion"["y"/"x"]
                    |in particular, $orig = forall ?. $concl["y"/?]
                    |""".stripMargin
                )
        }
    else outOfBound(lineNr)

    private def tryVerifyForallImpElim(impLine: Int, assLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(impLine, assLine)) then
        (lmap(impLine), lmap(assLine)) match {
            // imp = forall x. ass[?/x] -> concl[?/x]
            case (Pf(Forall(x, Implies(assF, conclF)), _, _), Pf(ass, _, _))
                if isSubstitutionOf(ass, assF, x) &&
                    isSubstitutionOf(concl, conclF, x) &&
                    (ass.getVars() removedAll assF.getVars()) ==
                    (concl.getVars() removedAll conclF.getVars()) =>
                Success(Nil)
            case (i, a) =>
                Failure(
                  s"""
                    |rule ${input.rule} expects "implication" = forall "x". "assumption"[?/x] -> "conclusion"[?/x]
                    |in particular, $i = forall "x". ($a)[?/x] -> ($concl)[?/x]
                    |""".stripMargin
                )
        }
    else outOfBound(lineNr)

    private def tryVerifyLEM()(using
        input: Pf,
        concl: LFormula,
        env: Set[String]
    ) = concl match {
        // concl = x / ~x
        // x bounded
        case Or(t @ PredAp(Predicate(x, 0), Nil), notT)
            if Not(t) == notT && env(x) =>
            Success(Nil)
        case _ =>
            Failure(
              s"rule ${input.rule} expects conclusion ($concl) to be of form x / ~x"
            )
    }

    private def tryVerifyMT(impLine: Int, negatedLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(impLine, negatedLine)) then
        (lmap(impLine), lmap(negatedLine)) match {
            // imp = x -> y
            // concl = ~x
            // not = ~y
            case (Pf(Implies(left, right), _, _), Pf(Not(x), _, _))
                if right == x && concl == Not(left) =>
                Success(Nil)
            case (implies, negated) =>
                Failure(s"""
                    |rule ${input.rule} expects "implication" = "x" -> "y"...
                    |and "conclusion" = ~"x"...
                    |and "negated" = ~"y"
                    |In particular, 
                    |   "conclusion" = $concl
                    |   "implication" = $implies
                    |   "negated" = $negated
                    |But the relations are not satisfied
                |""".stripMargin)
        }
    else outOfBound(lineNr)

    private def tryVerifyPC(negatedLine: Int, bottomLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(negatedLine, bottomLine)) then
        (lmap(negatedLine), lmap(bottomLine)) match {
            // bottom = F
            // concl = ~orig
            case (Pf(negated, _, _), Pf(Falsity(), _, _))
                if Not(concl) == negated =>
                Success(Nil)
            case (negated, bottom) =>
                Failure(s"""
                    |rule ${input.rule} expects "bottom" = "F" and "conclusion" = ~"original"
                    |In particular,
                    |   "conclusion" = $concl
                    |   "bottom" = $bottom
                    |   "negated" = $negated
                    |But the relations are not satisfied
            |""".stripMargin)
        }
    else outOfBound(lineNr)

    private def tryVerifyRefl()(using
        input: Pf,
        concl: LFormula,
        env: Set[String]
    ) = concl match {
        // concl = a = a
        // a bounded
        // WARN: not sure if we should expect a predAp
        // TODO: support funcAp
        case Eq(Variable(l), Variable(r)) if l == r && env(l) =>
            Success(Nil)
        case _ =>
            Failure(s"""
                    |rule ${input.rule} expects "conclusion" ($concl) = "x" = "x"
                |""".stripMargin)
    }

    private def tryVerifyEqSub(origLine: Int, eqLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(origLine, eqLine)) then
        (lmap(origLine), lmap(eqLine)) match {
            // eq = a = b
            // concl = orig[a/b]
            case (Pf(orig, _, _), Pf(Eq(Variable(a), Variable(b)), _, _))
                if orig.substitute(a, b) == concl ||
                    orig.substitute(b, a) == concl =>
                Success(Nil)
            case (orig, eq) =>
                Failure(s"""
                        |rule ${input.rule} expects "conclusion"[x/y] = "original"
                        |"eq" = x = y
                        |In particular,
                        |   "eq" = $eq
                        |   "conclusion" = $concl
                        |   "original" = $orig
                        |But the relations are not satisfied
                |""".stripMargin)
        }
    else outOfBound(lineNr)

    private def tryVerifySym(eqLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(eqLine)) then
        lmap(eqLine) match {
            // eq = a = b
            // concl = b = a
            case (Pf(Eq(l, r), _, _)) if concl == Eq(r, l) =>
                Success(Nil)
            case (eq) =>
                Failure(s"""
                        |rule ${input.rule} expects "eq" = x = y, "conclusion" = y = x
                        |In particular,
                        |   "eq" = $eq
                        |   "conclusion" = $concl
                        |But the relations are not satisfied
                |""".stripMargin)
        }
    else outOfBound(lineNr)

    private def tryVerifyForallIConst()(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line],
        env: Set[String]
    ) = concl match {
        case PredAp(Predicate(c, 0), Nil) if !env(c) =>
            Success(List(c))
        case _ =>
            Failure(
              s"rule ${input.rule} expects \"conclusion\" to be a single free variable"
            )
    }

    private def tryVerifyTick(origLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(origLine)) then
        lmap(origLine) match {
            case Pf(orig, _, _) if orig == concl =>
                Success(Nil)
            case orig =>
                Failure(
                  s"rule ${input.rule} expects \"conclusion\" ($concl) to be equal to \"original\" ($orig)"
                )
        }
    else outOfBound(lineNr)
}
