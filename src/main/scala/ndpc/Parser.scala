package ndpc

import parsley.Parsley
import parsley.Parsley.{many, some, atomic, lookAhead, pure}
import parsley.character.{satisfy, char}
import parsley.syntax.character.{charLift, stringLift}
import parsley.debug._

import ndpc.Formula._

object FormulaParser {
    // utils
    val spc = many(' ')
    val keywords =
        Set('(', ')', ' ', '.', ',', '~', '=', '^', '/', '<', '-', '>')
    def isKeyword = keywords.contains(_)
    val ident = some(satisfy(!isKeyword(_))).map(_.mkString)
    // format: off
    def args[A](one: Parsley[A]): Parsley[List[A]] =
        '(' ~> spc ~>
        // 0 arity
        (')'.as(List[A]()) <|>
        // 1+ arity
        (one <~> many(spc ~> ',' ~> spc ~> one <~ spc) <~ ')').map {
            (c: A, cs: List[A]) => c :: cs
        })
    // format: on

    // LTerm
    val variable = ident.map(LTerm.Variable.apply)
    lazy val funcAp: Parsley[LTerm.FuncAp] =
        (
          ident <~ spc <~> args(lterm)
        ).map { (res: (String, List[LTerm])) =>
            LTerm.FuncAp(
              Function(res._1, res._2.length),
              res._2
            )
        }

    // funcAp needs to have a higher precedence (or the function name is parsed as a variable!)
    lazy val lterm = atomic(funcAp) <|> variable

    // LFormula
    // we introduce a bit of syntax sugar here, if a predicate has arity 0,
    // you can omit the parenthesis, this makes propositional logic strictly
    // a subset of first ordet logic in our syntax system.
    lazy val predAp: Parsley[LFormula.PredAp] =
        (
          (ident <~ spc) <~>
              (args(lterm) <|> pure(List()))
        ).map { (res: (String, List[LTerm])) =>
            LFormula.PredAp(
              Predicate(res._1, res._2.length),
              res._2
            )
        }
    val equ =
        (lterm <~> (spc ~> '=' ~> spc) ~> lterm).map { (res: (LTerm, LTerm)) =>
            LFormula.Eq(res._1, res._2)
        }
    val truth =
        ('T' <~ atomic(lookAhead(satisfy(isKeyword)))).as(LFormula.Truth)
    val falsity =
        ('F' <~ atomic(lookAhead(satisfy(isKeyword)))).as(LFormula.Falsity)
    lazy val not = ('~' ~> spc ~> lformula).map(LFormula.Not.apply)
    // format: off
    lazy val connectives =
        (lformula <**>
            (spc ~> (
              '^'.as((l: LF_) => (r: LF_) => LFormula.And(l, r)) <|>
              '/'.as((l: LF_) => (r: LF_) => LFormula.Or(l, r)) <|>
              "->".as((l: LF_) => (r: LF_) => LFormula.Implies(l, r)) <|>
              "<->".as((l: LF_) => (r: LF_) => LFormula.Equiv(l, r))
            ) <~ spc)
            <*> lformula)
    // format: on
    lazy val forall =
        (("forall" ~> spc ~> some(ident <~ spc) <~ '.') <~> lformula)
            .map { (res: (List[String], LF_)) =>
                LFormula.Forall(res._1, res._2)
            }
    lazy val exists =
        (("exists" ~> spc ~> some(ident <~ spc) <~ '.') <~> lformula)
            .map { (res: (List[String], LF_)) =>
                LFormula.Exists(res._1, res._2)
            }
    lazy val lformula: Parsley[LF_] =
        truth <|>
            falsity <|>
            atomic(forall) <|>
            atomic(exists) <|>
            atomic(equ) <|>
            atomic(predAp) <|>
            atomic(not) <|>
            atomic(connectives)
}

object Parser {
    def parse(input: String) = ???
}
