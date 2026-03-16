package ndpc.frontend.parsers

import ndpc.frontend.expr.formula._
import ndpc.frontend.parsers.utils._
import ndpc.frontend.parsers.lexer.{identifier, symbol, lexeme}
import ndpc.frontend.parsers.lexer.implicits.implicitSymbol

import parsley.Parsley
import parsley.Parsley.{pure, notFollowedBy, atomic}
import parsley.combinator.sepBy
import parsley.syntax.character.charLift
import parsley.expr.{precedence, Ops, InfixL, Prefix}
import parsley.errors.combinator._
import parsley.debug._

object FormulaParser {

    // LFormula
    // we introduce a bit of syntax sugar here, if a predicate has arity 0,
    // you can omit the parenthesis.
    // we don't really need the original semantics anyway

    val predAps =
        symbol.openParen ~> lexeme(
          sepBy(lexeme(predAp), lexeme(','))
        ) <~ symbol.closingParen
    // lazy val predAp: Parsley[PredAp] =
    //     (
    //       (lexeme(identifier)) <~> (predAps <|> pure(Nil))
    //     )
    //         .label("predicate application")
    //         .map { (res: (String, List[PredAp])) =>
    //             PredAp(res._1, res._2)
    //         }
    lazy val predAp: Parsley[LFormula] = PredAp(
      lexeme(identifier),
      predAps <|> pure(Nil)
    ).label("predicate application")
    val equ = Eq(predAp, "=" ~> predAp).label("equality")
    // T followed by some keyword
    val truth =
        (symbol.softKeyword("T").label("truth") as Truth)
            <~ notFollowedBy('(')
    // F followed by some keyword
    val falsity =
        (symbol.softKeyword("F").label("falsity") as Falsity)
            <~ notFollowedBy('(')
    // format: off
    val atom: Parsley[LFormula] = (
        atomic(truth) <|>
        atomic(falsity) <|>
        atomic(equ) <|>
        atomic(predAp)
    ).label("Atom (T/F/equality/predicate application)")
    lazy val lformula: Parsley[LFormula] = (
        // "atom"-s connected by connectives
        precedence(
            tolerant(atom) <|>
            tolerant(symbol.openParen ~> tolerant(lformula) <~ symbol.closingParen)
        )(
            Ops(InfixL)(Eq from "="),
            Ops(Prefix)(Not from "~"),
            Ops(Prefix)("forall" ~> identifier.map(ident => Forall(ident, _)) <~ "."),
            Ops(Prefix)("exists" ~> identifier.map(ident => Exists(ident, _)) <~ "."),
            Ops(InfixL)(And from "^"),
            Ops(InfixL)(Or from "/"),
            Ops(InfixL)(Implies from "->"),
            Ops(InfixL)(Equiv from "<->")
        )
    ).label("Lformula (forall statement/exists statement/lformula and logical connectives)")
    // format: on
}
