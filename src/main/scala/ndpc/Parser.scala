package ndpc

import parsley.Parsley
import parsley.Parsley.{many, some, atomic, lookAhead, pure}
import parsley.character.{satisfy, char}
import parsley.syntax.character.{charLift, stringLift}
import parsley.character.whitespaces as spc
import parsley.combinator.sepBy
import parsley.debug._

import ndpc.Formula._

// utils
val keywords = Set(
  '(', ')', '[', ']', '<', '>', ' ', '.', ',', '~', '=', '^', '/', '-'
)
def isKeyword = keywords.contains(_)
val ident = some(satisfy(!isKeyword(_))).map(_.mkString)
def resolve(exprs: List[LF_ | (LF_ => (LF_ => LF_))]): LF_ = ???

object FormulaParser {

    // LTerm
    val variable = ident.map(LTerm.Variable.apply)
    val args = '(' ~> spc ~> sepBy(lterm, spc ~> ',' <~ spc) <~ ')'
    lazy val funcAp: Parsley[LTerm.FuncAp] =
        (
          ident <~ spc <~> args
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
              (args <|> pure(List()))
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
    lazy val not = ???
    // TODO: parse to IR
    lazy val connectives = ???
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
    // format: off
    lazy val lformula: Parsley[LF_] =
        // "atoms"
        atomic(predAp) <|>
        atomic(truth) <|>
        atomic(falsity) <|>
        // "atoms" bracketed
        ('(' ~> spc ~> lformula <~ spc <~ ')') <|>
        atomic(forall) <|>
        atomic(exists) <|>
        // "atoms" connected by connectives
        ???
    // format: on
}

object Parser {
    def parse(input: String) = ???
}
