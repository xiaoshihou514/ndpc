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
    private val numbers =
        tolerant(args(number)).label("numbers separated by comma")
    private def unary(
        kw: String,
        to: Int => Rule[Int]
    ): Parsley[Rule[Int]] =
        tolerant(
          (kw ~> tolerant(arg(number)).map(to))
        )
    private def binary(
        kw: String,
        to: ((Int, Int)) => Rule[Int]
    ): Parsley[Rule[Int]] =
        tolerant((kw ~> tolerant(args(number))).map { (l: List[Int]) =>
            to((l(0), l(1)))
        })

    val rule: Parsley[Rule[Int]] =
        "LEM".as(LEM[Int]()).label("Law of Exclueded Middle") <|>
            "refl".as(Refl[Int]()).label("REFLective rule") <|>
            "given".as(Given[Int]()).label("Given") <|>
            "premise".as(Premise[Int]()).label("Premise") <|>
            "ass".as(Ass[Int]()).label("ASSume") <|>
            "TI".as(TruthIntro[Int]()).label("Truth Introduction") <|>
            // forall I const
            atomic(
              "forall" ~> some(' ') ~> 'I' ~> some(' ') ~> "const".as(
                (ForallIConst[Int]())
              )
            ).label("Forall constant introduction") <|>
            binary("MT", MT[Int].apply).label("Modus Tollens") <|>
            binary("PC", PC[Int].apply).label("Proof by Contradiction") <|>
            binary("=sub", EqSub[Int].apply).label("substitution") <|>
            unary("sym", Sym[Int].apply).label("rule of SYMmetry") <|>
            unary("tick", Tick[Int].apply).label("the 'Tick'") <|>
            atomic(binary("^I", AndIntro[Int].apply))
                .label("And introduction") <|>
            unary("^E", AndElim[Int].apply).label("And elimination") <|>
            atomic(binary("->I", ImpliesIntro[Int].apply))
                .label("Implication introduction") <|>
            binary("->E", ImpliesElim[Int].apply)
                .label("Implication elimination") <|>
            atomic(unary("/I", OrIntro[Int].apply)).label("Or introduction") <|>
            binary("/E", OrElim[Int].apply).label("Or Elimination") <|>
            // format: off
            // ~~E and ~~I
            atomic(
              "~~" ~> (
                'E'.as((prev: Int) => DoubleNegElim(prev))
                .label("Double negation elimination") <|>
                'I'.as((prev: Int) => DoubleNegIntro(prev))
                .label("Double negation introduction")) <~> arg(number)
            ).map { (res: (Int => Rule[Int], Int)) => res._1(res._2) } <|>
            // ~E and ~I
            atomic(
              "~" ~> (
                'E'.as((list: List[Int]) => NotElim(list(0), list(1)))
                .label("Not elimination") <|>
                'I'.as((list: List[Int]) => NotIntro(list(0), list(1)))
                .label("Not introduction")) <~> numbers
            ).map { (res: (List[Int] => Rule[Int], List[Int])) =>
                res._1(res._2)
            } <|>
            // FE and FI
            atomic("FE") ~> numbers.map {(res: List[Int]) => FalsityElim(res(0))} <|>
            "FI" ~> numbers.map{(res: List[Int]) => FalsityIntro(res(0), res(1))} <|>
            // <->E and <->I
            // somehow if I change the order it won't work...
            (
              "<->" ~> (
                  'I'.as((list: List[Int]) => EquivIntro(list(0), list(1)))
                  .label("Equiv introduction") <|>
                  'E'.as((list: List[Int]) => EquivElim(list(0), list(1))))
                  .label("Equiv elimination") <~> numbers
            ).map { (res: (List[Int] => Rule[Int], List[Int])) =>
                res._1(res._2)
            } <|>
            // format: on
            atomic(unary("existsI", ExistsIntro.apply))
                .label("Exists introduction") <|>
            binary("existsE", ExistsElim.apply).label("Exists elimination") <|>
            atomic(binary("forallI", ForallIntro.apply))
                .label("Forall elimination") <|>
            atomic(unary("forallE", ForallElim.apply))
                .label("Forall introduction") <|>
            atomic(binary("forall->E", ForallImpElim.apply))
                .label("Forall implies elimination")
}
