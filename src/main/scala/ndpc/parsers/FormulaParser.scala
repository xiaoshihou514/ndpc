package ndpc.parsers

import parsley.Parsley
import parsley.Parsley.{some, atomic, lookAhead, pure, eof}
import parsley.character.satisfy
import parsley.syntax.character.charLift
import parsley.expr.{precedence, Ops, InfixL, Prefix}
import parsley.errors.combinator._
import parsley.debug._

import ndpc.expr.Formula._
import ndpc.parsers.Utils._
import ndpc.parsers.Lexer.{identifier, symbol}
import ndpc.parsers.Lexer.implicits.implicitSymbol

object FormulaParser {
    // LTerm
    val variable = identifier.map(Variable.apply).label("variable")
    val lterms = args(lterm)
    lazy val funcAp: Parsley[FuncAp] =
        (identifier <~ spc <~> lterms)
            .label("function application")
            .map { (res: (String, List[LTerm])) =>
                FuncAp(
                  Function(res._1, res._2.length),
                  res._2
                )
            }

    // funcAp needs to have a higher precedence (or the function name is parsed as a variable!)
    lazy val lterm = (atomic(funcAp) <|> variable).label(
      "lterm (function application or variable)"
    )

    // LFormula
    // we introduce a bit of syntax sugar here, if a predicate has arity 0,
    // you can omit the parenthesis, this makes propositional logic strictly
    // a subset of first ordet logic in our syntax system.
    lazy val predAp: Parsley[PredAp] =
        (
          (identifier <~ spc) <~> (lterms <|> pure(List()))
        )
            .label("predicate application")
            .map { (res: (String, List[LTerm])) =>
                PredAp(
                  Predicate(res._1, res._2.length),
                  res._2
                )
            }
    val equ =
        (lterm <~> "=" ~> lterm)
            .label("equality")
            .map { (res: (LTerm, LTerm)) =>
                Eq(res._1, res._2)
            }
    // T followed by some keyword
    val truth = symbol.softKeyword("T").label("truth") as (Truth())
    // F followed by some keyword
    val falsity = symbol.softKeyword("F").label("falsity") as (Falsity())
    // format: off
    lazy val forall =
        (("forall" ~> some(identifier) <~ ".") <~> lformula)
        .label("forall statement")
        .map { (res: (List[String], LF_)) =>
            Forall(res._1, res._2)
        }
    lazy val exists =
        (("exists" ~> some(identifier) <~ ".")
        <~> lformula)
        .label("exists statement")
        .map { (res: (List[String], LF_)) =>
            Exists(res._1, res._2)
        }
    val atom: Parsley[LF_] = (
        atomic(truth) <|>
        atomic(falsity) <|>
        atomic(equ) <|>
        atomic(predAp)
    ).label("Atom (T/F/equality/predicate application)")
    .asInstanceOf[Parsley[LF_]] // come on scala, you can do this!
    lazy val lformula: Parsley[LF_] = (
        atomic(forall) <|>
        atomic(exists) <|>
        // "atom"-s connected by connectives
        precedence(
            tolerant('(' ~> tolerant(lformula) <~ ')') <|>
            tolerant(atom)
        )(
            Ops(Prefix)("~" as Not.apply),
            Ops(InfixL)("^" as And.apply),
            Ops(InfixL)("/" as Or.apply),
            Ops(InfixL)("->" as Implies.apply),
            Ops(InfixL)("<->" as Equiv.apply)
        )
    ).label("Lformula (forall statement/exists statement/lformula and logical connectives)")
    // format: on
}
