package ndpc

import parsley.Parsley
import parsley.Parsley.{many, some, atomic, lookAhead, pure, eof}
import parsley.character.{satisfy, char}
import parsley.syntax.character.{charLift, stringLift}
import parsley.combinator.sepBy
import parsley.expr.{precedence, Ops, InfixL, Prefix}
import parsley.debug._

import ndpc.Formula._

// utils
val keywords = Set(
  '(', ')', '[', ']', '<', '>', ' ', '.', ',', '~', '=', '^', '/', '-'
)
val spc = many(' ')
def isKeyword = keywords.contains(_)
val ident = some(satisfy(!isKeyword(_))).map(_.mkString)
def tolerant[A](p: Parsley[A]): Parsley[A] = spc ~> p <~ spc
// need to hint scala about the type we want
val neg: (LF_ => LF_) = LFormula.Not.apply
val and: ((LF_, LF_) => LF_) = LFormula.And.apply
val or: ((LF_, LF_) => LF_) = LFormula.Or.apply
val implies: ((LF_, LF_) => LF_) = LFormula.Implies.apply
val equiv: ((LF_, LF_) => LF_) = LFormula.Equiv.apply

object FormulaParser {
    // LTerm
    val variable = ident.map(LTerm.Variable.apply)
    val args =
        '(' ~> tolerant(sepBy(tolerant(lterm), ',')) <~ ')'
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
          (ident <~ spc) <~> (args <|> pure(List()))
        ).map { (res: (String, List[LTerm])) =>
            LFormula.PredAp(
              Predicate(res._1, res._2.length),
              res._2
            )
        }
    val equ =
        (lterm <~> tolerant('=') ~> lterm).map { (res: (LTerm, LTerm)) =>
            LFormula.Eq(res._1, res._2)
        }
    // T followed by some keyword
    val truth =
        ('T' <~ atomic(
          lookAhead(satisfy(isKeyword)) <|> eof
        )) as (LFormula.Truth)
    // F followed by some keyword
    val falsity =
        ('F' <~ atomic(
          lookAhead(satisfy(isKeyword)) <|> eof
        )) as (LFormula.Falsity)
    lazy val not = ???
    lazy val forall =
        (("forall" ~> spc ~> some(ident <~ some(' ')) <~ '.') <~> lformula)
            .map { (res: (List[String], LF_)) =>
                LFormula.Forall(res._1, res._2)
            }
    lazy val exists =
        (("exists" ~> spc ~> some(ident <~ some(' ')) <~ '.') <~> lformula)
            .map { (res: (List[String], LF_)) =>
                LFormula.Exists(res._1, res._2)
            }
    // format: off
    val atom: Parsley[LF_] =
        atomic(truth) <|>
        atomic(falsity) <|>
        atomic(predAp)
    lazy val lformula: Parsley[LF_] =
        atomic(forall) <|>
        atomic(exists) <|>
        // "atom"-s connected by connectives
        precedence(
            tolerant('(' ~> tolerant(lformula) <~ ')') <|>
            tolerant(atom)
        )(
            Ops(Prefix)(tolerant('~') as neg),
            Ops(InfixL)(tolerant('^') as and),
            Ops(InfixL)(tolerant('/') as or),
            Ops(InfixL)(tolerant("->") as implies),
            Ops(InfixL)(tolerant("<->") as equiv)
        )
    // format: on
}

object Parser {
    def parse(input: String) = ???
}
