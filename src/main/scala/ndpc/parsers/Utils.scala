package ndpc.parsers

import parsley.Parsley
import parsley.Parsley.many
import parsley.syntax.character.{charLift, stringLift}
import parsley.combinator.sepBy

object Utils {
    val spc = many(' ')
    def tolerant[A](p: Parsley[A]): Parsley[A] = spc ~> p <~ spc

    def arg[A](one: Parsley[A]) = tolerant('(') ~> one <~ tolerant(')')
    def args[A](one: Parsley[A]) =
        '(' ~> tolerant(sepBy(tolerant(one), ',')) <~ ')'
}
