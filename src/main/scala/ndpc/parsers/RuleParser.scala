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
        to: BigInt => Rule[BigInt]
    ): Parsley[Rule[BigInt]] =
        tolerant(
          (kw ~> tolerant(arg(number)).map(to))
        )
    private def binary(
        kw: String,
        to: ((BigInt, BigInt)) => Rule[BigInt]
    ): Parsley[Rule[BigInt]] =
        tolerant((kw ~> tolerant(args(number))).map { (l: List[BigInt]) =>
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
          Special.MT[BigInt].apply.tupled.andThen(Rule.Builtin[BigInt].apply)
        ).label("Modus Tollens") <|>
        binary(
          "PC",
          Special.PC[BigInt].apply.tupled.andThen(Rule.Builtin[BigInt].apply)
        ).label("Proof by Contradiction") <|>
        binary(
          "=sub",
          Special.EqSub[BigInt].apply.tupled.andThen(Rule.Builtin[BigInt].apply)
        ).label("substitution") <|>
        unary(
          "sym",
          Special.Sym[BigInt].apply.andThen(Rule.Builtin[BigInt].apply)
        ).label("rule of SYMmetry") <|>
        unary(
          "tick",
          Special.Tick[BigInt].apply.andThen(Rule.Builtin[BigInt].apply)
        ).label("the 'Tick'") <|>
        atomic(
          binary(
            "^I",
            Introduction
                .And[BigInt]
                .apply
                .tupled
                .andThen(Rule.Intro[BigInt].apply)
          )
        ).label("And introduction") <|>
        unary(
          "^E",
          Elimination.And[BigInt].apply.andThen(Rule.Elim[BigInt].apply)
        ).label("And elimination") <|>
        atomic(
          binary(
            "->I",
            Introduction
                .Implies[BigInt]
                .apply
                .tupled
                .andThen(Rule.Intro[BigInt].apply)
          )
        ).label("Implication introduction") <|>
        binary(
          "->E",
          Elimination
              .Implies[BigInt]
              .apply
              .tupled
              .andThen(Rule.Elim[BigInt].apply)
        ).label("Implication elimination") <|>
        atomic(
          unary(
            "/I",
            Introduction.Or[BigInt].apply.andThen(Rule.Intro[BigInt].apply)
          )
        ).label("Or introduction") <|>
        binary(
          "/E",
          Elimination.Or[BigInt].apply.tupled.andThen(Rule.Elim[BigInt].apply)
        ).label("Or Elimination") <|>
        // ~~E and ~~I
        atomic(
          "~~" ~> ('E'
              .as((prev: BigInt) => Rule.Elim(Elimination.DoubleNeg(prev)))
              .label("Double negation elimination") <|>
              'I'.as((prev: BigInt) => Rule.Intro(Introduction.DoubleNeg(prev)))
                  .label("Double negation introduction")) <~> arg(number)
        ).map { (res: (BigInt => Rule[BigInt], BigInt)) => res._1(res._2) } <|>
        // ~E and ~I
        atomic(
          "~" ~> ('E'
              .as((list: List[BigInt]) =>
                  Rule.Elim(Elimination.Not(list(0), list(1)))
              )
              .label("Not elimination") <|>
              'I'.as((list: List[BigInt]) =>
                  Rule.Intro(Introduction.Not(list(0), list(1)))
              ).label("Not introduction")) <~> numbers
        ).map { (res: (List[BigInt] => Rule[BigInt], List[BigInt])) =>
            res._1(res._2)
        } <|>
        // FE and FI
        ('F' ~> ('E'
            .as((i: BigInt, j: BigInt) => Rule.Elim(Elimination.Falsity(i, j)))
            .label("Falsity elimination") <|>
            'I'.as((i: BigInt, j: BigInt) =>
                Rule.Intro(Introduction.Falsity(i, j))
            ).label("Falsity introduction")) <~> numbers).map {
            (res: ((BigInt, BigInt) => Rule[BigInt], List[BigInt])) =>
                res._1(res._2(0), res._2(1))
        } <|>
        // <->E and <->I
        (
          "<->" ~> ('E'
              .as((list: List[BigInt]) =>
                  Rule.Elim(Elimination.Equiv(list(0), list(1)))
              )
              .label("Equiv elimination") <|>
              'I'.as((list: List[BigInt]) =>
                  Rule.Intro(Introduction.Equiv(list(0), list(1)))
              ).label("Equiv introduction")) <~> numbers
        ).map { (res: (List[BigInt] => Rule[BigInt], List[BigInt])) =>
            res._1(res._2)
        } <|>
        atomic(
          unary(
            "existsI",
            Introduction.Exists[BigInt].apply.andThen(Rule.Intro[BigInt].apply)
          )
        ).label("Exists introduction") <|>
        binary(
          "existsE",
          Elimination
              .Exists[BigInt]
              .apply
              .tupled
              .andThen(Rule.Elim[BigInt].apply)
        ).label("Exists elimination") <|>
        atomic(
          binary(
            "forallI",
            Introduction
                .Forall[BigInt]
                .apply
                .tupled
                .andThen(Rule.Intro[BigInt].apply)
          )
        ).label("Forall elimination") <|>
        atomic(
          unary(
            "forallE",
            Elimination.Forall[BigInt].apply.andThen(Rule.Elim[BigInt].apply)
          )
        ).label("Forall introduction") <|>
        atomic(
          binary(
            "forall->E",
            Elimination
                .ForallImp[BigInt]
                .apply
                .tupled
                .andThen(Rule.Elim[BigInt].apply)
          )
        ).label("Forall implies elimination")
}
