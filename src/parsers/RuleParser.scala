package ndpc.parsers

import parsley.Parsley
import parsley.Parsley.{many, some, atomic}
import parsley.character.digit
import parsley.syntax.character.{charLift, stringLift}
import parsley.combinator.sepBy
import parsley.errors.combinator._
import parsley.debug._

import ndpc.expr.Rule._
import ndpc.parsers.Utils._
import ndpc.parsers.Lexer._

object RuleParser {
    private def unary(
        kw: String,
        to: Int => Rule
    ): Parsley[Rule] =
        tolerant(
          (kw ~> tolerant(arg(number)).map(to))
        )
    private def nNums(n: Int) = tolerant(nargs(number, n))
    private def binary(
        kw: String,
        to: ((Int, Int)) => Rule
    ): Parsley[Rule] =
        tolerant((kw ~> tolerant(nNums(2))).map { (l: List[Int]) =>
            to((l(0), l(1)))
        })

    val rule: Parsley[Rule] =
        "LEM".as(LEM()).label("Law of Exclueded Middle") <|>
            "refl".as(Refl()).label("REFLective rule") <|>
            "given".as(Given()).label("Given") <|>
            "premise".as(Premise()).label("Premise") <|>
            "ass".as(Ass()).label("ASSume") <|>
            "TI".as(TruthIntro()).label("Truth Introduction") <|>
            // forall I const
            atomic(
              "forall" ~> some(' ') ~> 'I' ~> some(' ') ~> "const".as(
                (ForallIConst())
              )
            ).label("Forall constant introduction") <|>
            binary("MT", MT.apply).label("Modus Tollens") <|>
            binary("PC", PC.apply).label("Proof by Contradiction") <|>
            binary("=sub", EqSub.apply).label("substitution") <|>
            unary("sym", Sym.apply).label("rule of SYMmetry") <|>
            unary("tick", Tick.apply).label("the 'Tick'") <|>
            atomic(binary("^I", AndIntro.apply))
                .label("And introduction") <|>
            unary("^E", AndElim.apply).label("And elimination") <|>
            atomic(binary("->I", ImpliesIntro.apply))
                .label("Implication introduction") <|>
            binary("->E", ImpliesElim.apply)
                .label("Implication elimination") <|>
            atomic(unary("/I", OrIntro.apply)).label("Or introduction") <|>
            tolerant(("/E" ~> tolerant(nNums(5))).map { (l: List[Int]) =>
                OrElim(l(0), l(1), l(2), l(3), l(4))
            }).label("Or Elimination") <|> 
            // format: off
            // ~~E and ~~I
            atomic(
              "~~" ~> (
                lexeme('E').as((prev: Int) => DoubleNegElim(prev))
                .label("Double negation elimination") <|>
                lexeme('I').as((prev: Int) => DoubleNegIntro(prev))
                .label("Double negation introduction")) <*> arg(number)
            ) <|>
            // ~E and ~I
            atomic(
              "~" ~> (
                lexeme('E').as((list: List[Int]) => NotElim(list(0), list(1)))
                .label("Not elimination") <|>
                lexeme('I').as((list: List[Int]) => NotIntro(list(0), list(1)))
                .label("Not introduction")) <*> nNums(2)
            ) <|>
            // FE and FI
            atomic("FE") ~> arg(number).map {(res: Int) => FalsityElim(res)} <|>
            "FI" ~> nNums(2).map{(res: List[Int]) => FalsityIntro(res(0), res(1))} <|>
            // <->E and <->I
            // somehow if I change the order it won't work...
            (
              "<->" ~> (
                  lexeme('I').as((list: List[Int]) => EquivIntro(list(0), list(1)))
                  .label("Equiv introduction") <|>
                  lexeme('E').as((list: List[Int]) => EquivElim(list(0), list(1))))
                  .label("Equiv elimination") <*> nNums(2)
            ) <|>
            // format: on
            atomic(unary("existsI", ExistsIntro.apply))
                .label("Exists introduction") <|>
            tolerant(("existsE" ~> tolerant(nNums(3))).map { (l: List[Int]) =>
                ExistsElim(l(0), l(1), l(2))
            }).label("Exists elimination") <|>
            atomic(binary("forallI", ForallIntro.apply))
                .label("Forall elimination") <|>
            atomic(unary("forallE", ForallElim.apply))
                .label("Forall introduction") <|>
            atomic(binary("forall->E", ForallImpElim.apply))
                .label("Forall implies elimination")
}
