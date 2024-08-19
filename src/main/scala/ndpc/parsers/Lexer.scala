package ndpc.parsers

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.descriptions.{LexicalDesc, NameDesc, SymbolDesc, SpaceDesc}
import parsley.errors.combinator._

object Lexer {
    private val ops = Set(
      '(', ')', '[', ']', '<', '>', ' ', '.', ',', '~', '=', '^', '/', '-'
    )
    private val spaces = Set(' ', '\t')
    private val desc = LexicalDesc.plain.copy(
      nameDesc = NameDesc.plain.copy(
        // let's do basic for now
        identifierStart = predicate.Basic(!ops(_)),
        identifierLetter = predicate.Basic(!ops(_))
      ),
      // \n is significant!
      spaceDesc = SpaceDesc.plain.copy(
        space = predicate.Basic(spaces)
      ),
      symbolDesc = SymbolDesc.plain.copy(
        hardKeywords = Set("forall", "exists"),
        // format: off
        hardOperators = Set(
          "^",
          "/",
          "->",
          "<->",
          "~",
          "--",
          "~~",
          ",",
          ".",
          "(", ")",
          "[", "]",
          "<", ">"
        )
        // format: on
      )
    )

    private val lexer = new Lexer(desc)

    val identifier = lexer.lexeme.names.identifier
    val number = lexer.lexeme.natural.decimal.label("number")
    val symbol = lexer.lexeme.symbol

    def fully[A](p: Parsley[A]) = lexer.fully(p)
    val implicits = lexer.lexeme.symbol.implicits
}
