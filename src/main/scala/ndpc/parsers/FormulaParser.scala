package ndpc.parsers

import parsley.Parsley
import parsley.Parsley.{some, atomic, lookAhead, pure, eof, notFollowedBy}
import parsley.combinator.sepBy
import parsley.character.satisfy
import parsley.syntax.character.charLift
import parsley.expr.{precedence, Ops, InfixL, Prefix}
import parsley.errors.combinator._
import parsley.debug._

import ndpc.expr.Formula._
import ndpc.parsers.Utils._
import ndpc.parsers.Lexer.{identifier, symbol, lexeme}
import ndpc.parsers.Lexer.implicits.implicitSymbol

object FormulaParser {

    // LFormula
    // we introduce a bit of syntax sugar here, if a predicate has arity 0,
    // you can omit the parenthesis.
    // we don't really need the original semantics anyway

    val predAps =
        symbol.openParen ~> lexeme(
          sepBy(lexeme(predAp), lexeme(','))
        ) <~ symbol.closingParen
    lazy val predAp: Parsley[PredAp] =
        (
          (lexeme(identifier)) <~> (predAps <|> pure(Nil))
        )
            .label("predicate application")
            .map { (res: (String, List[PredAp])) =>
                PredAp(res._1, res._2)
            }
    val equ =
        (predAp <~> "=" ~> predAp)
            .label("equality")
            .map { (res: (PredAp, PredAp)) =>
                Eq(res._1, res._2)
            }
    // T followed by some keyword
    val truth =
        (symbol.softKeyword("T").label("truth") as Truth())
            <~ notFollowedBy('(')
    // F followed by some keyword
    val falsity =
        (symbol.softKeyword("F").label("falsity") as Falsity())
            <~ notFollowedBy('(')
    // format: off
    val atom: Parsley[LFormula] = (
        atomic(truth) <|>
        atomic(falsity) <|>
        atomic(equ) <|>
        atomic(predAp)
    ).label("Atom (T/F/equality/predicate application)")
    .asInstanceOf[Parsley[LFormula]] // come on scala, you can do this!
    lazy val lformula: Parsley[LFormula] = (
        // "atom"-s connected by connectives
        precedence(
            tolerant(atom) <|>
            tolerant(symbol.openParen ~> tolerant(lformula) <~ symbol.closingParen)
        )(
            Ops(InfixL)("=" as Eq.apply),
            Ops(Prefix)("~" as Not.apply),
            Ops(Prefix)(("forall" ~> identifier <~ ".") <**> pure(ident => f => Forall(ident, f))),
            Ops(Prefix)(("exists" ~> identifier <~ ".") <**> pure(ident => f => Exists(ident, f))),
            Ops(InfixL)("^" as And.apply),
            Ops(InfixL)("/" as Or.apply),
            Ops(InfixL)("->" as Implies.apply),
            Ops(InfixL)("<->" as Equiv.apply)
        )
    ).label("Lformula (forall statement/exists statement/lformula and logical connectives)")
    // format: on
}
