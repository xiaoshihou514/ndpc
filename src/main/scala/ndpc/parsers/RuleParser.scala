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

    val rule = "LEM"
        .as(Rule.Builtin(Special.LEM()))
        .label("Law of Exclueded Middle") <|>
        "refl".as(Rule.Builtin(Special.Refl())).label("REFLective rule") <|>
        "given".as(Rule.Builtin(Special.Given())).label("Given") <|>
        "premise".as(Rule.Builtin(Special.Premise())).label("Premise") <|>
        "ass".as(Rule.Builtin(Special.Ass())).label("ASSume") <|>
        "TI".as(Rule.Intro(Introduction.Truth()))
            .label("Truth Introduction") <|>
        // forall I const
        atomic(
          "forall" ~> some(' ') ~> 'I' ~> some(' ') ~> "const".as(
            Rule.Builtin(Special.ForallIConst())
          )
        ).label("Forall constant introduction") <|>
        binary(
          "MT",
          Special.MT[Int].apply.tupled.andThen(Rule.Builtin[Int].apply)
        ).label("Modus Tollens") <|>
        binary(
          "PC",
          Special.PC[Int].apply.tupled.andThen(Rule.Builtin[Int].apply)
        ).label("Proof by Contradiction") <|>
        binary(
          "=sub",
          Special.EqSub[Int].apply.tupled.andThen(Rule.Builtin[Int].apply)
        ).label("substitution") <|>
        unary(
          "sym",
          Special.Sym[Int].apply.andThen(Rule.Builtin[Int].apply)
        ).label("rule of SYMmetry") <|>
        unary(
          "tick",
          Special.Tick[Int].apply.andThen(Rule.Builtin[Int].apply)
        ).label("the 'Tick'") <|>
        atomic(
          binary(
            "^I",
            Introduction
                .And[Int]
                .apply
                .tupled
                .andThen(Rule.Intro[Int].apply)
          )
        ).label("And introduction") <|>
        unary(
          "^E",
          Elimination.And[Int].apply.andThen(Rule.Elim[Int].apply)
        ).label("And elimination") <|>
        atomic(
          binary(
            "->I",
            Introduction
                .Implies[Int]
                .apply
                .tupled
                .andThen(Rule.Intro[Int].apply)
          )
        ).label("Implication introduction") <|>
        binary(
          "->E",
          Elimination
              .Implies[Int]
              .apply
              .tupled
              .andThen(Rule.Elim[Int].apply)
        ).label("Implication elimination") <|>
        atomic(
          unary(
            "/I",
            Introduction.Or[Int].apply.andThen(Rule.Intro[Int].apply)
          )
        ).label("Or introduction") <|>
        binary(
          "/E",
          Elimination.Or[Int].apply.tupled.andThen(Rule.Elim[Int].apply)
        ).label("Or Elimination") <|>
        // ~~E and ~~I
        atomic(
          "~~" ~> ('E'
              .as((prev: Int) => Rule.Elim(Elimination.DoubleNeg(prev)))
              .label("Double negation elimination") <|>
              'I'.as((prev: Int) => Rule.Intro(Introduction.DoubleNeg(prev)))
                  .label("Double negation introduction")) <~> arg(number)
        ).map { (res: (Int => Rule[Int], Int)) => res._1(res._2) } <|>
        // ~E and ~I
        atomic(
          "~" ~> ('E'
              .as((list: List[Int]) =>
                  Rule.Elim(Elimination.Not(list(0), list(1)))
              )
              .label("Not elimination") <|>
              'I'.as((list: List[Int]) =>
                  Rule.Intro(Introduction.Not(list(0), list(1)))
              ).label("Not introduction")) <~> numbers
        ).map { (res: (List[Int] => Rule[Int], List[Int])) =>
            res._1(res._2)
        } <|>
        // FE and FI
        ('F' ~> ('E'
            .as((i: Int, j: Int) => Rule.Elim(Elimination.Falsity(i, j)))
            .label("Falsity elimination") <|>
            'I'.as((i: Int, j: Int) => Rule.Intro(Introduction.Falsity(i, j)))
                .label("Falsity introduction")) <~> numbers).map {
            (res: ((Int, Int) => Rule[Int], List[Int])) =>
                res._1(res._2(0), res._2(1))
        } <|>
        // <->E and <->I
        (
          "<->" ~> ('E'
              .as((list: List[Int]) =>
                  Rule.Elim(Elimination.Equiv(list(0), list(1)))
              )
              .label("Equiv elimination") <|>
              'I'.as((list: List[Int]) =>
                  Rule.Intro(Introduction.Equiv(list(0), list(1)))
              ).label("Equiv introduction")) <~> numbers
        ).map { (res: (List[Int] => Rule[Int], List[Int])) =>
            res._1(res._2)
        } <|>
        atomic(
          unary(
            "existsI",
            Introduction.Exists[Int].apply.andThen(Rule.Intro[Int].apply)
          )
        ).label("Exists introduction") <|>
        binary(
          "existsE",
          Elimination
              .Exists[Int]
              .apply
              .tupled
              .andThen(Rule.Elim[Int].apply)
        ).label("Exists elimination") <|>
        atomic(
          binary(
            "forallI",
            Introduction
                .Forall[Int]
                .apply
                .tupled
                .andThen(Rule.Intro[Int].apply)
          )
        ).label("Forall elimination") <|>
        atomic(
          unary(
            "forallE",
            Elimination.Forall[Int].apply.andThen(Rule.Elim[Int].apply)
          )
        ).label("Forall introduction") <|>
        atomic(
          binary(
            "forall->E",
            Elimination
                .ForallImp[Int]
                .apply
                .tupled
                .andThen(Rule.Elim[Int].apply)
          )
        ).label("Forall implies elimination")
}
