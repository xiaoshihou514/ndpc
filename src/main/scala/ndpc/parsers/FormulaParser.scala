package ndpc.parsers

import parsley.Parsley
import parsley.Parsley.{some, atomic, lookAhead, pure, eof}
import parsley.character.satisfy
import parsley.syntax.character.charLift
import parsley.expr.{precedence, Ops, InfixL, Prefix}
import parsley.debug._

import ndpc.expr.Formula._
import ndpc.parsers.Utils._
import ndpc.parsers.Lexer.{identifier, symbol}
import ndpc.parsers.Lexer.implicits.implicitSymbol

object FormulaParser {
    // utils
    // need to hint scala about the type we want
    private val neg: (LF_ => LF_) = LFormula.Not.apply
    private val and: ((LF_, LF_) => LF_) = LFormula.And.apply
    private val or: ((LF_, LF_) => LF_) = LFormula.Or.apply
    private val implies: ((LF_, LF_) => LF_) = LFormula.Implies.apply
    private val equiv: ((LF_, LF_) => LF_) = LFormula.Equiv.apply

    // LTerm
    val variable = identifier.map(LTerm.Variable.apply)
    val lterms = args(lterm)
    lazy val funcAp: Parsley[LTerm.FuncAp] =
        (
          identifier <~ spc <~> lterms
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
          (identifier <~ spc) <~> (lterms <|> pure(List()))
        ).map { (res: (String, List[LTerm])) =>
            LFormula.PredAp(
              Predicate(res._1, res._2.length),
              res._2
            )
        }
    val equ =
        (lterm <~> "=" ~> lterm).map { (res: (LTerm, LTerm)) =>
            LFormula.Eq(res._1, res._2)
        }
    // T followed by some keyword
    val truth = symbol.softKeyword("T") as (LFormula.Truth)
    // F followed by some keyword
    val falsity = symbol.softKeyword("F") as (LFormula.Falsity)
    // format: off
    lazy val forall =
        (("forall" ~> some(identifier) <~ ".")
        <~> lformula)
        .map { (res: (List[String], LF_)) =>
            LFormula.Forall(res._1, res._2)
        }
    lazy val exists =
        (("exists" ~> some(identifier) <~ ".")
        <~> lformula)
        .map { (res: (List[String], LF_)) =>
            LFormula.Exists(res._1, res._2)
        }
    val atom: Parsley[LF_] =
        atomic(truth) <|>
        atomic(falsity) <|>
        atomic(equ) <|>
        atomic(predAp)
    lazy val lformula: Parsley[LF_] =
        atomic(forall) <|>
        atomic(exists) <|>
        // "atom"-s connected by connectives
        precedence(
            tolerant('(' ~> tolerant(lformula) <~ ')') <|>
            tolerant(atom)
        )(
            Ops(Prefix)("~" as neg),
            Ops(InfixL)("^" as and),
            Ops(InfixL)("/" as or),
            Ops(InfixL)("->" as implies),
            Ops(InfixL)("<->" as equiv)
        )
    // format: on
}
