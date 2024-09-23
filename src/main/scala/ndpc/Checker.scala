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

    def check(inputs: List[String], toJson: Boolean): Int = {
        val errors = pfFromSource(inputs)
            .filter(_.isFailure)
            .asInstanceOf[List[Failure[NdpcError]]]
        if !errors.isEmpty then
            if toJson then printErrorJson(errors)
            else printErrorHuman(errors)
        else ok("All proofs are valid!")
        errors.length
    }

    // I really want consistent error handling here so I went for java exceptions,
    // which is well captured by scala.util.Try
    private def pfFromSource(
        inputs: List[String]
    ): List[Result[NdpcError, CheckedProof]] =
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
                            throw new ParserException(
                              fromStringError(reason.asInstanceOf[String])
                            )
                    }
                }
                .map { (upf: UncheckedProof) =>
                    checkOne(upf) match {
                        case Success(pf) => pf
                        case Failure(reason) =>
                            throw new CheckException(reason.asInstanceOf[EnrichedErr])
                    }
                } match {
                case scala.util.Success(pf) => Success(pf)
                case scala.util.Failure(exception) => {
                    exception match {
                        case ParserException(reason) =>
                            Failure(
                              SyntaxError(reason.copy(file = Some(input)))
                            )
                        case CheckException(reason) =>
                            Failure(
                              SemanticsError(reason.copy(file = Some(input)))
                            )
                        case throwable @ _ =>
                            Failure(IOError(input, throwable.toString()))
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

    private def checkOne(upf: UncheckedProof): Result[EnrichedErr, CheckedProof] = {
        val pfs = upf.main.body.dropWhile(x => isPremise(x) || isComment(x))
        val maybeTangling = PfScope(pfs).flatten().find(isPremise)
        if !maybeTangling.isEmpty then
            Failure(
              EnrichedErr(
                "Did not expect \"Assume\" and \"Premise\" to be used in places other than the start of proof",
                None,
                Some(upf.lines.indexOf(maybeTangling.get))
              )
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
    ): Result[EnrichedErr, Int] = boundary {
        // it's readable, but pretty ugly IMO
        // verify head
        val head = input.body
            .dropWhile(x => isComment(x))
            .head
        head match {
            case Pf(_, Ass() | ForallIConst(), _) => // pass
            case Pf(_, Given() | Premise(), _)    => // pass
            case pf @ Pf(_, _, _) =>
                boundary.break(
                  Failure(
                    EnrichedErr(
                      s"Box starting at line $lineNr did not start with a valid proof "
                          + "(expected assumption, forall I const, given, premise)",
                      None,
                      Some(lines.indexOf(head))
                    )
                  )
                )
            case _ => // pass
        }

        val tail = input.body.reverse
            .dropWhile(isComment(_))
            .head
        tail match {
            case scope @ PfScope(_) =>
                boundary.break(
                  Failure(
                    EnrichedErr(
                      "Box ended with another box",
                      None,
                      Some(lines.indexOf(tail))
                    )
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
                              Failure(
                                EnrichedErr(
                                  reason.asInstanceOf[String],
                                  None,
                                  Some(lineNr + offset)
                                )
                              )
                            )
                        case Success(vars) =>
                            env addAll vars
                            if line.isInstanceOf[Pf] then localKnowledge add line.asInstanceOf[Line]
                    }
                    offset = offset + 1
            }
        }

        // we should leave with nothing but the conclusion
        // we can also reuse the variables, but let's omit that for brecity
        // it's possible that this scope is main and we did not begin with any premises
        if head.isInstanceOf[Line] then
            boxConcls add (
              head.asInstanceOf[Line],
              tail.asInstanceOf[Line],
            )
        Success(offset)
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
                    case FalsityIntro(orig, negated) =>
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
                    case ForallImpElim(ass, imp) => 
                        tryVerifyForallImpElim(ass, imp)

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

    private def inBound(it: Int, current: Int) = it < current && it > 0

    private def verifyArgs(args: List[Int])(using
        lineNr: Int,
        lines: List[Line],
        knowledgeBase: Set[Line]
    ) = args.forall(x => inBound(x, lineNr) && knowledgeBase(lines(x - 1)))

    private def lmap(lineNumber: Int)(using lines: List[Line]) =
        lines(lineNumber - 1)

    private def filterOOB(lineNrs: List[Int])(using
        lineNr: Int,
        lines: List[Line],
        knowledgeBase: Set[Line]
    ) = lineNrs.filter(x => !inBound(x, lineNr) || !knowledgeBase(lines(x - 1)))

    private def outOfBound(lineNrs: List[Int])(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        knowledgeBase: Set[Line]
    ) = Failure(
      s"Line numbers ${filterOOB(lineNrs).mkString(", ")} " +
          "referenced in ${input.rule} are not accessible to line $lineNr."
    )

    private def notProofs(lines: List[(Int, Line)])(using input: Pf) =
        Failure(
          ("The following line(s) referenced in ${input.rule} are not proofs:" ::
              lines
                  .filter((_, l) => !l.isInstanceOf[Pf])
                  .map((n, l) => s"  line $n: $l"))
              .mkString("\n")
        )

    private def buildError(
        assertions: List[(Boolean, String)],
        context: List[(String, LFormula)]
    )(using input: Pf): Failure[String] = Failure(
      ((
        s"The following assertion(s) implied by ${input.rule} does not hold:" ::
            assertions.filter(!_._1).map("  " + _._2)
      ) ++ (
        "In particular with the following variables:" ::
            context.map((desc, f) => s"  $desc: $f")
      )).mkString("\n")
    )

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
                case (Pf(left, _, _), Pf(right, _, _)) =>
                    if (concl == And(left, right)) then Success(Nil)
                    else
                        buildError(
                          List(
                            (concl == And(left, right)) -> s"Conclusion equal to left ^ right"
                          ),
                          List(
                            "Conclusion" -> concl,
                            "Left" -> left,
                            "Right" -> right
                          )
                        )
                case (left, right) =>
                    notProofs(List(leftLine -> left, rightLine -> right))
            }
        else outOfBound(List(leftLine, rightLine))

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
                case (a @ Pf(ass, _, _), r @ Pf(res, _, _)) =>
                    if (concl == Implies(ass, res) && boxConcls((a, r))) then Success(Nil)
                    else
                        buildError(
                          List(
                            (concl == Implies(ass, res)) ->
                                s"Conclusion equal to Assumption -> Implication",
                            boxConcls((a, r)) ->
                                s"Assumption and Implication be the assumption and conclusion of a box"
                          ),
                          List(
                            "Conclusion" -> concl,
                            "Assumption" -> ass,
                            "Implication" -> res
                          )
                        )
                case (ass, res) =>
                    notProofs(List(assLine -> ass, resLine -> res))
            }
        else outOfBound(List(assLine, resLine))

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
                case (Pf(either, _, _), Or(left, right)) if either == left || either == right =>
                    Success(Nil)
                case (Pf(either, _, _), _) =>
                    buildError(
                      List(
                        concl.isInstanceOf[Or] -> "Conclusion is of form a / b",
                        false -> "Either is either side of Conclusion"
                      ),
                      List(
                        "Conclusion" -> concl,
                        "Either" -> either
                      )
                    )
                case (either, _) =>
                    notProofs(List(eitherLine -> either))
            }
        else outOfBound(List(eitherLine))

    private def tryVerifyNotIntro(origLine: Int, bottomLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        boxConcls: Set[(Line, Line)],
        knowledge: Set[Line]
    ) =
        if verifyArgs(List(origLine, bottomLine)) then
            (lmap(origLine), lmap(bottomLine)) match {
                // concl = ~orig
                // bottom = F
                case (ol @ Pf(orig, _, _), bl @ Pf(Falsity(), _, _))
                    if concl == Not(orig) && boxConcls((ol, bl)) =>
                    Success(Nil)
                case (ol @ Pf(orig, _, _), bl @ Pf(bottom, _, _)) =>
                    buildError(
                      List(
                        (concl == Not(orig)) -> "Conclusion equal to ~(Original)",
                        (bottom == Falsity()) -> "Bottom equal to F",
                        boxConcls((ol, bl)) ->
                            "Original and Bottom is the assumption and conclusion of a box"
                      ),
                      List(
                        "Conclusion" -> concl,
                        "Original" -> orig,
                        "Bottom" -> bottom
                      )
                    )
                case (orig, bottom) =>
                    notProofs(List(origLine -> orig, bottomLine -> bottom))
            }
        else outOfBound(List(origLine, bottomLine))

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
                case Pf(orig, _, _) =>
                    if concl == Not(Not(orig)) then Success(Nil)
                    else
                        buildError(
                          List(
                            (concl == Not(Not(orig))) -> "Conclusion equal to ~(~(Original))"
                          ),
                          List(
                            "Conclusion" -> concl,
                            "Original" -> orig
                          )
                        )
                case orig =>
                    notProofs(List(origLine -> orig))
            }
        else outOfBound(List(origLine))

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
                case (Pf(orig, _, _), Pf(negated, _, _)) =>
                    // we don't allow orig and negated to be reversed, same for the others
                    if (negated == Not(orig) && concl == Falsity()) then Success(Nil)
                    else
                        buildError(
                          List(
                            (negated == Not(orig)) -> "Negated equal to ~(Original)",
                            (concl == Falsity()) -> "Conclusion equal to F"
                          ),
                          List(
                            "Conclusion" -> concl,
                            "Negated" -> negated,
                            "Original" -> orig
                          )
                        )
                case (orig, negated) =>
                    notProofs(List(origLine -> orig, negatedLine -> negated))
            }
        else outOfBound(List(origLine, negatedLine))

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
                case (Pf(leftImp, _, _), Pf(rightImp, _, _)) =>
                    buildError(
                      List(
                        leftImp.isInstanceOf[Implies] -> "LeftImplication is an implication",
                        rightImp.isInstanceOf[Implies] -> "RightImplication is an implication",
                        false -> "LeftImplication and RightImplication has their assumptions and implications reversed"
                      ),
                      List(
                        "Conclusion" -> concl,
                        "LeftImplication" -> leftImp,
                        "RightImplication" -> rightImp
                      )
                    )
                case (l, r) =>
                    notProofs(List(leftImpLine -> l, rightImpLine -> r))
            }
        else outOfBound(List(leftImpLine, rightImpLine))

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
                // x free in orig
                case (Pf(orig, _, _), Exists(x, conclF))
                    if !orig.getVars()(x) &&
                        isSubstituteOf(orig, conclF, x) =>
                    // if the above holds then concl won't have any free variables
                    // the proof is left as an exercise
                    Success(Nil)
                case (Pf(orig, _, _), _) =>
                    buildError(
                      List(
                        concl.isInstanceOf[Exists] -> "Conclusion be of form exists ?. A",
                        (
                          concl.isInstanceOf[Exists] &&
                              !orig.getVars()(concl.asInstanceOf[Exists].x)
                        ) -> "Original free of Conclusion's quantifier",
                        (
                          concl.isInstanceOf[Exists] &&
                              isSubstituteOf(
                                orig,
                                concl.asInstanceOf[Exists].body,
                                concl.asInstanceOf[Exists].x
                              )
                        ) -> "Body of Conclusion is Original with one variable substituted"
                      ),
                      List(
                        "Conclusion" -> concl,
                        "Original" -> orig
                      )
                    )
                case (orig, _) =>
                    notProofs(List(origLine -> orig))
            }
        else outOfBound(List(origLine))

    private def tryVerifyForallIntro(
        constLine: Int,
        conclForallLine: Int,
        env: Set[String]
    )(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line],
        boxConcls: Set[(Line, Line)]
    ) =
        if verifyArgs(List(constLine, conclForallLine)) then
            (lmap(constLine), lmap(conclForallLine), concl) match {
                // const = c
                // concl = forall x. conclF[c/x]
                // x free in conclF
                case (
                      cl @ Pf(c @ PredAp(_, Nil), _, _),
                      ccl @ Pf(conclF, _, _),
                      Forall(x, f)
                    )
                    if conclF.substitutes(c, PredAp(x, Nil))(f) &&
                        !conclF.getVars()(x) && boxConcls((cl, ccl)) =>
                    Success(Nil)
                case (cl @ Pf(c, _, _), ccl @ Pf(conclF, _, _), _) =>
                    buildError(
                      List(
                        (c.isInstanceOf[PredAp] && c.asInstanceOf[PredAp].args == Nil)
                            -> "C is a constant",
                        concl.isInstanceOf[Forall] -> "Conclusion is of form forall ?. A",
                        (
                          concl.isInstanceOf[Forall] &&
                              !conclF.getVars()(concl.asInstanceOf[Forall].x)
                        ) -> "ForallConclusion free of quantifier in Conclusion",
                        (
                          concl.isInstanceOf[Forall] &&
                              conclF.substitutes(c, PredAp(concl.asInstanceOf[Forall].x, Nil))(
                                concl.asInstanceOf[Forall].body
                              )
                        ) -> "Conclusion if of form forall x. ForallConclusion[?/x]",
                        boxConcls((cl, ccl)) ->
                            "C and ForallConclusion is assumption and conclusion of a box"
                      ),
                      List(
                        "C" -> c,
                        "Conclusion" -> concl,
                        "ForallConclusion" -> conclF
                      )
                    )
                case (c, fa, _) =>
                    notProofs(List(constLine -> c, conclForallLine -> fa))
            }
        else outOfBound(List(constLine, conclForallLine))

    private def isSubstituteOf(
        original: LFormula,
        substituted: LFormula,
        x: String
    ): Boolean =
        original == substituted ||
            original
                .getVars()
                // original[t/x] == substituted?
                .exists(t =>
                    original.substitutes(
                      PredAp(t, Nil),
                      PredAp(x, Nil)
                    )(substituted)
                )

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
                case Pf(And(left, right), _, _) if left == concl || right == concl =>
                    Success(Nil)
                case Pf(and, _, _) =>
                    buildError(
                      List(
                        and.isInstanceOf[And] -> "And be of form x ^ y",
                        false -> "Conclusion is either side of And"
                      ),
                      List(
                        "Conclusion" -> concl,
                        "And" -> and
                      )
                    )
                case orig =>
                    notProofs(List(origLine -> orig))
            }
        else outOfBound(List(origLine))

    private def tryVerifyImpliesElim(impLine: Int, assLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) =
        if verifyArgs(List(assLine, impLine)) then
            (lmap(assLine), lmap(impLine)) match {
                // imp = ass -> concl
                case (Pf(ass, _, _), Pf(imp, _, _)) =>
                    if imp == Implies(ass, concl) then Success(Nil)
                    else
                        buildError(
                          List(
                            (imp == Implies(ass, concl)) ->
                                "Implication equal to Assumption -> Conclusion"
                          ),
                          List(
                            "Conclusion" -> concl,
                            "Assumption" -> ass,
                            "Implication" -> imp
                          )
                        )
                case (ass, imp) =>
                    notProofs(List(assLine -> ass, impLine -> imp))
            }
        else outOfBound(List(assLine, impLine))

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
                      la @ Pf(leftAss, _, _),
                      lc @ Pf(leftConcl, _, _),
                      ra @ Pf(rightAss, _, _),
                      rc @ Pf(rightConcl, _, _)
                    ) =>
                    if boxConcls((la, lc)) &&
                        boxConcls((ra, rc)) &&
                        leftConcl == rightConcl &&
                        leftConcl == concl &&
                        or == Or(leftAss, rightAss)
                    then Success(Nil)
                    else
                        buildError(
                          List(
                            boxConcls(la, lc) ->
                                "LeftAssumption and LeftConclusion is assumption and conclusion of a box",
                            boxConcls(ra, rc) ->
                                "RightAssumption and RightConclusion is assumption and conclusion of a box",
                            (leftConcl == rightConcl && rightConcl == concl) ->
                                "LeftConclusion equals RightConclusion equals Conclusion",
                            (or == Or(leftAss, rightAss)) ->
                                "Or statement equals LeftAssumption / RightAssumption"
                          ),
                          List(
                            "Or" -> or,
                            "LeftAssumption" -> leftAss,
                            "RightAssumtion" -> rightAss,
                            "LeftConclusion" -> leftConcl,
                            "RightConclusion" -> rightConcl,
                            "Conclusion" -> concl
                          )
                        )
                case (or, la, lc, ra, rc) =>
                    notProofs(
                      List(
                        orLine -> or,
                        leftAssLine -> la,
                        leftConclLine -> lc,
                        rightAssLine -> ra,
                        rightConclLine -> rc
                      )
                    )
            }
        else
            outOfBound(
              List(
                orLine,
                leftAssLine,
                rightAssLine,
                leftConclLine,
                rightConclLine
              )
            )

    private def tryVerifyNotElim(negatedLine: Int, origLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(origLine, negatedLine)) then
        (lmap(origLine), lmap(negatedLine)) match {
            // negated = ~orig
            // concl = F
            case (Pf(orig, _, _), Pf(negated, _, _)) =>
                if negated == Not(orig) && concl == Falsity() then Success(Nil)
                else
                    buildError(
                      List(
                        (negated == Not(orig)) -> "Negated equals ~Original",
                        (concl == Falsity()) -> "Conclusion equals F"
                      ),
                      List(
                        "Conclusion" -> concl,
                        "Original" -> orig,
                        "Negated" -> negated
                      )
                    )
            case (orig, negated) =>
                notProofs(List(origLine -> orig, negatedLine -> negated))
        }
    else outOfBound(List(origLine, negatedLine))

    private def tryVerifyDoubleNegElim(origLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(origLine)) then
        lmap(origLine) match {
            // orig = ~~concl
            case Pf(orig, _, _) =>
                if orig == Not(Not(concl)) then Success(Nil)
                else
                    buildError(
                      List((orig == Not(Not(concl))) -> "Original equals ~(~(Conclusion))"),
                      List(
                        "Conclusion" -> concl,
                        "Original" -> orig
                      )
                    )
            case orig =>
                notProofs(List(origLine -> orig))
        }
    else outOfBound(List(origLine))

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
            case Pf(Falsity(), _, _) =>
                Success(concl.getVars().toList)
            case Pf(bottom, _, _) =>
                buildError(
                  List(false -> "Bottom equals F"),
                  List("Bottom" -> bottom)
                )
            case b =>
                notProofs(List(bottomLine -> b))
        }
    else outOfBound(List(bottomLine))

    private def tryVerifyEquivElim(equivLine: Int, eitherLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(equivLine, eitherLine)) then
        (lmap(equivLine), lmap(eitherLine)) match {
            // equiv = either <-> concl OR equiv = concl <-> either
            case (Pf(equiv, _, _), Pf(either, _, _)) =>
                if equiv == Equiv(concl, either) ||
                    equiv == Equiv(either, concl)
                then Success(Nil)
                else
                    buildError(
                      List(
                        false -> "Equiv equals Conclusion <-> Either or Either <-> Conclusion"
                      ),
                      List(
                        "Conclusion" -> concl,
                        "Either" -> either
                      )
                    )
            case (equiv, either) =>
                notProofs(List(equivLine -> equiv, eitherLine -> either))
        }
    else outOfBound(List(equivLine, eitherLine))

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
                    !ass.getVars()(x) &&
                    isSubstituteOf(ass, assE, x) =>
                (ass.getVars() removedAll assE.getVars()).toList match {
                    case Nil                              => Success(Nil)
                    case t :: Nil if !conclE.getVars()(t) => Success(Nil)
                    case _ =>
                        buildError(
                          List(
                            false -> s"Conclusion's body equals Assumption[?/$x] and ? free"
                          ),
                          List(
                            "Conclusion" -> concl,
                            "Assumption" -> ass
                          )
                        )
                }
            case (Pf(exists, _, _), al @ Pf(ass, _, _), cl @ Pf(conclE, _, _)) =>
                buildError(
                  List(
                    (concl == conclE) -> "Conclusion equals ExistsConclusion",
                    boxConcls((al, cl)) ->
                        "Assumption and ExistsConclusion is the assumption and conclusion of a box",
                    exists.isInstanceOf[Exists] -> "Exists is of form exists ?. A",
                    (
                      exists.isInstanceOf[Exists] &&
                          isSubstituteOf(
                            ass,
                            exists.asInstanceOf[Exists].body,
                            exists.asInstanceOf[Exists].x
                          )
                    ) -> "Exists' body equals Assumption[?/?] and its quantifier is free in Assumption"
                  ),
                  List(
                    "Conclusion" -> concl,
                    "Exists" -> exists,
                    "ExistsConclusion" -> conclE,
                    "Assumption" -> ass
                  )
                )
            case (exists, ass, _) =>
                notProofs(List(existsLine -> exists, assLine -> ass))
        }
    else outOfBound(List(existsLine, assLine, conclELine))

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
            case Pf(forall @ Forall(x, conclF), _, _) if isSubstituteOf(concl, conclF, x) =>
                (concl.getVars() removedAll conclF.getVars()).toList match {
                    case Nil                => Success(Nil)
                    case t :: Nil if env(t) => Success(Nil)
                    case _ =>
                        buildError(
                          List(
                            false -> "Conclusion is Forall's conclusion with quantifier substituted"
                          ),
                          List(
                            "Conclusion" -> concl,
                            "Forall" -> forall
                          )
                        )
                }
            case Pf(forall, _, _) =>
                buildError(
                  List(
                    forall.isInstanceOf[Forall] -> "Forall is of form forall ?. A",
                    false -> "Conclusion is Forall's body with quantifier substituted"
                  ),
                  List(
                    "Conclusion" -> concl,
                    "Forall" -> forall
                  )
                )
            case orig => notProofs(List(origLine -> orig))
        }
    else outOfBound(List(origLine))

    private def tryVerifyForallImpElim(assLine: Int, impLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line]
    ) = if verifyArgs(List(impLine, assLine)) then
        (lmap(impLine), lmap(assLine)) match {
            // imp = forall x. ass[?/x] -> concl[?/x]
            case (Pf(fi @ Forall(x, Implies(assF, conclF)), _, _), Pf(ass, _, _)) =>
                if isSubstituteOf(ass, assF, x) &&
                    isSubstituteOf(concl, conclF, x) &&
                    (ass.getVars() removedAll assF.getVars()) ==
                        (concl.getVars() removedAll conclF.getVars())
                then Success(Nil)
                else
                    buildError(
                      List(
                        isSubstituteOf(ass, assF, x) ->
                            "Assumption is assumption in ForallImplies' body with quantifier substituted",
                        isSubstituteOf(concl, conclF, x) ->
                            "Conclusion is conclusion in ForallImplies' body with quantifier substituted",
                        (
                          (ass.getVars() removedAll assF.getVars()) ==
                              (concl.getVars() removedAll conclF.getVars())
                        ) -> "Assumption and Conclusion uses the same variable for quantifier substituion"
                      ),
                      List(
                        "ForallImplies" -> fi,
                        "Conclusion" -> concl,
                        "Assumption" -> ass
                      )
                    )
            case (Pf(fi, _, _), Pf(_, _, _)) =>
                buildError(
                  List(false -> "ForallImplies be of form forall ?. (A -> B)"),
                  List("ForallImplies" -> fi)
                )
            case (i, a) =>
                notProofs(List(assLine -> a, impLine -> i))
        }
    else outOfBound(List(impLine, assLine))

    private def tryVerifyLEM()(using
        input: Pf,
        concl: LFormula,
        env: Set[String]
    ) = concl match {
        // concl = x / ~x
        case Or(t @ PredAp(_, _), notT) if Not(t) == notT =>
            Success(Nil)
        case _ =>
            buildError(
              List(false -> "Conclusion is of form x / ~x"),
              List("Conclusion" -> concl)
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
            case (Pf(implies, _, _), Pf(not, _, _)) =>
                buildError(
                  List(
                    implies.isInstanceOf[Implies] -> "Implies is of form x -> y",
                    not.isInstanceOf[Not] -> "Not is of form ~(A)",
                    false -> "Implies equals A -> B, where ~A equals Conclusion and ~B equals Not"
                  ),
                  List(
                    "Conclusion" -> concl,
                    "Implies" -> implies,
                    "Not" -> not
                  )
                )
            case (implies, negated) =>
                notProofs(List(impLine -> implies, negatedLine -> negated))
        }
    else outOfBound(List(impLine, negatedLine))

    private def tryVerifyPC(negatedLine: Int, bottomLine: Int)(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        boxConcls: Set[(Line, Line)],
        knowledge: Set[Line]
    ) = if verifyArgs(List(negatedLine, bottomLine)) then
        (lmap(negatedLine), lmap(bottomLine)) match {
            // bottom = F
            // concl = ~orig
            case (nl @ Pf(negated, _, _), fl @ Pf(f, _, _)) =>
                if f == Falsity() &&
                    Not(concl) == negated &&
                    boxConcls((nl, fl))
                then Success(Nil)
                else
                    buildError(
                      List(
                        (f == Falsity()) -> "Bottom equals F",
                        (Not(concl) == negated) -> "Negated equals ~(Conclusion)",
                        boxConcls((nl, fl)) ->
                            "Negated and Bottom is the assumption and conclusion of a box"
                      ),
                      List(
                        "Conclusion" -> concl,
                        "Bottom" -> f,
                        "Negated" -> negated
                      )
                    )
            case (negated, bottom) =>
                notProofs(List(negatedLine -> negated, bottomLine -> bottom))
        }
    else outOfBound(List(negatedLine, bottomLine))

    private def tryVerifyRefl()(using
        input: Pf,
        concl: LFormula,
        env: Set[String]
    ) = concl match {
        // concl = a = a
        // a bounded
        case Eq(l @ PredAp(_, _), r @ PredAp(_, _)) if l == r && l.getVars().forall(env) =>
            Success(Nil)
        case _ =>
            buildError(
              List(false -> "Conclusion if of form x = x, where x is (recursively) bounded"),
              List("Conclusion" -> concl)
            )
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
            case (Pf(orig, _, _), Pf(Eq(a, b), _, _))
                if orig.substitutes(a, b)(concl) ||
                    orig.substitutes(b, a)(concl) =>
                Success(Nil)
            case (Pf(orig, _, _), Pf(eq, _, _)) =>
                buildError(
                  List(
                    eq.isInstanceOf[Eq] -> "Equality is of form x = y",
                    false -> "Conclusion is a valid substitution of Original, with substituted variables from Equality"
                  ),
                  List(
                    "Conclusion" -> concl,
                    "Equality" -> eq
                  )
                )
            case (orig, eq) =>
                notProofs(List(origLine -> orig, eqLine -> eq))
        }
    else outOfBound(List(origLine, eqLine))

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
            case (Pf(eq, _, _)) =>
                buildError(
                  List(false -> "Conclusion is Equality but sides reversed"),
                  List(
                    "Conclusion" -> concl,
                    "Equality" -> eq
                  )
                )
            case (eq) => notProofs(List(eqLine -> eq))
        }
    else outOfBound(List(eqLine))

    private def tryVerifyForallIConst()(using
        input: Pf,
        lineNr: Int,
        lines: List[Line],
        concl: LFormula,
        knowledge: Set[Line],
        env: Set[String]
    ) = concl match {
        case PredAp(c, Nil) if !env(c) =>
            Success(List(c))
        case _ =>
            buildError(
              List(false -> "Conclusion is a single variable"),
              List("Conclusion" -> concl)
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
            case Pf(orig, _, _) =>
                if orig == concl then Success(Nil)
                else
                    buildError(
                      List(false -> "Conclusion equals Original"),
                      List(
                        "Conclusion" -> concl,
                        "Original" -> orig
                      )
                    )
            case orig =>
                notProofs(List(origLine -> orig))
        }
    else outOfBound(List(origLine))
}
