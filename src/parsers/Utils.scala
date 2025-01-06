package ndpc.parsers

import parsley.quick.{Parsley, many, pure, sepBy}
import parsley.syntax.character.{charLift, stringLift}
import ndpc.parsers.Lexer.{symbol, lexeme}

object Utils {
    val spc = many(' ' <|> '\t')
    def tolerant[A](p: Parsley[A]): Parsley[A] = spc ~> p <~ spc

    def arg[A](one: Parsley[A]) =
        symbol.openParen ~> lexeme(one) <~ symbol.closingParen

    def nargs[A](one: Parsley[A], n: Int): Parsley[List[A]] =
        symbol.openParen ~>
            nSepBy(lexeme(one), lexeme(','), n)
            <~ symbol.closingParen

    private def nSepBy[A, B](
        one: Parsley[A],
        sep: Parsley[B],
        rep: Int
    ): Parsley[List[A]] =
        if rep == 1 then one <::> pure(Nil)
        else (one <~ sep) <::> nSepBy(one, sep, rep - 1)
}
