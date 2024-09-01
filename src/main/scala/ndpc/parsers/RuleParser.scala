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

    val rule: Parsley[Rule[Int]] = "LEM"
        .as(LEM())
        .label("Law of Exclueded Middle") <|>
        "refl".as(Refl()).label("REFLective rule") <|>
        "given".as(Given()).label("Given") <|>
        "premise".as(Premise()).label("Premise") <|>
        "ass".as(Ass()).label("ASSume") <|>
        "TI".as(TruthIntro())
            .label("Truth Introduction") <|>
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
        atomic(binary("^I", AndIntro.apply)).label("And introduction") <|>
        unary("^E", AndElim[Int].apply).label("And elimination") <|>
        atomic(binary("->I", ImpliesIntro[Int].apply))
            .label("Implication introduction") <|>
        binary("->E", ImpliesElim[Int].apply)
            .label("Implication elimination") <|>
        atomic(unary("/I", OrIntro[Int].apply)).label("Or introduction") <|>
        binary("/E", OrElim[Int].apply).label("Or Elimination") <|>
        // ~~E and ~~I
        atomic(
          "~~" ~> ('E'
              .as((prev: Int) => DoubleNegElim(prev))
              .label("Double negation elimination") <|>
              'I'.as((prev: Int) => DoubleNegIntro(prev))
                  .label("Double negation introduction")) <~> arg(number)
        ).map { (res: (Int => Rule[Int], Int)) => res._1(res._2) } <|>
        // ~E and ~I
        atomic(
          "~" ~> ('E'
              .as((list: List[Int]) => NotElim(list(0), list(1)))
              .label("Not elimination") <|>
              'I'.as((list: List[Int]) => NotIntro(list(0), list(1)))
                  .label("Not introduction")) <~> numbers
        ).map { (res: (List[Int] => Rule[Int], List[Int])) =>
            res._1(res._2)
        } <|>
        // FE and FI
        ('F' ~> ('E'
            .as((i: Int, j: Int) => FalsityElim(i, j))
            .label("Falsity elimination") <|>
            'I'.as((i: Int, j: Int) => FalsityIntro(i, j))
                .label("Falsity introduction")) <~> numbers).map {
            (res: ((Int, Int) => Rule[Int], List[Int])) =>
                res._1(res._2(0), res._2(1))
        } <|>
        // <->E and <->I
        (
          "<->" ~> ('E'
              .as((list: List[Int]) => EquivElim(list(0), list(1))))
              .label("Equiv elimination") <|>
              'I'.as((list: List[Int]) => EquivIntro(list(0), list(1)))
                  .label("Equiv introduction") <~> numbers
        ).map { (res: (List[Int] => Rule[Int], List[Int])) =>
            res._1(res._2)
        } <|>
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
