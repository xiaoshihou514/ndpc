package ndpc.parsers

import parsley.Parsley
import parsley.Parsley.{many, some, atomic}
import parsley.character.digit
import parsley.syntax.character.{charLift, stringLift}
import parsley.combinator.sepBy
import parsley.debug._

import ndpc.expr.Rule._
import ndpc.parsers.Utils._
import ndpc.parsers.Lexer._

object RuleParser {
    private val numbers = tolerant(args(number))
    private def unary(kw: String, to: BigInt => Rule): Parsley[Rule] = tolerant(
      (kw ~> tolerant(arg(number)).map(to))
    )
    private def binary(
        kw: String,
        to: ((BigInt, BigInt)) => Rule
    ): Parsley[Rule] =
        tolerant((kw ~> tolerant(args(number))).map { (l: List[BigInt]) =>
            to((l(0), l(1)))
        })

    val rule = "LEM".as(Rule.Derived(Special.LEM)) <|>
        "refl".as(Rule.Derived(Special.Refl)) <|>
        "given".as(Rule.Derived(Special.Given)) <|>
        "ass".as(Rule.Derived(Special.Ass)) <|>
        "TI".as(Rule.Intro(Introduction.Truth)) <|>
        binary(
          "MT",
          Special.MT.apply.tupled.andThen(Rule.Derived.apply)
        ) <|>
        binary(
          "PC",
          Special.PC.apply.tupled.andThen(Rule.Derived.apply)
        ) <|>
        binary(
          "=sub",
          Special.EqSub.apply.tupled.andThen(Rule.Derived.apply)
        ) <|>
        unary("sym", Special.Sym.apply.andThen(Rule.Derived.apply)) <|>
        unary(
          "tick",
          Special.Tick.apply.andThen(Rule.Derived.apply)
        ) <|>
        atomic(
          binary(
            "^I",
            Introduction.And.apply.tupled.andThen(Rule.Intro.apply)
          )
        ) <|>
        unary("^E", Elimination.And.apply.andThen(Rule.Elim.apply)) <|>
        atomic(
          binary(
            "->I",
            Introduction.Implies.apply.tupled.andThen(Rule.Intro.apply)
          )
        ) <|>
        binary(
          "->E",
          Elimination.Implies.apply.tupled.andThen(Rule.Elim.apply)
        ) <|>
        atomic(
          unary("/I", Introduction.Or.apply.andThen(Rule.Intro.apply))
        ) <|>
        binary(
          "/E",
          Elimination.Or.apply.tupled.andThen(Rule.Elim.apply)
        ) <|>
        // ~~E and ~~I
        atomic(
          "~~" ~> ('E'.as((prev: BigInt) =>
              Rule.Elim(Elimination.DoubleNeg(prev))
          ) <|> 'I'.as((prev: BigInt) =>
              Rule.Intro(Introduction.DoubleNeg(prev))
          )) <~> arg(number)
        ).map { (res: (BigInt => Rule, BigInt)) => res._1(res._2) } <|>
        // ~E and ~I
        atomic(
          "~" ~> ('E'.as((list: List[BigInt]) =>
              Rule.Elim(Elimination.Not(list(0), list(1)))
          ) <|> 'I'.as((list: List[BigInt]) =>
              Rule.Intro(Introduction.Not(list(0), list(1)))
          )) <~> numbers
        ).map { (res: (List[BigInt] => Rule, List[BigInt])) =>
            res._1(res._2)
        } <|>
        // FE and FI
        ('F' ~> ('E'.as((i: BigInt, j: BigInt) =>
            Rule.Elim(Elimination.Falsity(i, j))
        ) <|> 'I'.as((i: BigInt, j: BigInt) =>
            Rule.Intro(Introduction.Falsity(i, j))
        )) <~> numbers).map { (res: ((BigInt, BigInt) => Rule, List[BigInt])) =>
            res._1(res._2(0), res._2(1))
        } <|>
        // <->E and <->I
        (
          "<->" ~> ('E'.as((list: List[BigInt]) =>
              Rule.Elim(Elimination.Equiv(list(0), list(1)))
          ) <|> 'I'.as((list: List[BigInt]) =>
              Rule.Intro(Introduction.Equiv(list(0), list(1)))
          )) <~> numbers
        ).map { (res: (List[BigInt] => Rule, List[BigInt])) =>
            res._1(res._2)
        } <|>
        atomic(
          unary(
            "existsI",
            Introduction.Exists.apply.andThen(Rule.Intro.apply)
          )
        ) <|>
        binary(
          "existsE",
          Elimination.Exists.apply.tupled.andThen(Rule.Elim.apply)
        ) <|>
        atomic(
          binary(
            "forallI",
            Introduction.Forall.apply.tupled.andThen(Rule.Intro.apply)
          )
        ) <|>
        unary(
          "forallE",
          Elimination.Forall.apply.andThen(Rule.Elim.apply)
        )
}
