package ndpc

import parsley.Parsley, Parsley.{many, some, atomic}
import parsley.character.{satisfy, char}
import parsley.syntax.character.{charLift, stringLift}
import parsley.debug._

import ndpc.syntax.Formula._

object Parser {
    val spaces = many(char(' '))
    // LTerm
    val keywords = Set('(', ')', ' ', '.', ',')
    val ident = some(satisfy(!keywords.contains(_)))
    val variable = ident.map { (cs: List[Char]) => LTerm.Variable(cs.mkString) }
    lazy val funcAp: Parsley[LTerm.FuncAp] = 
        (
            ident <~ spaces <~> (
                '(' ~> spaces ~>
                (
                    // 0 arity
                    ')'.map(_ => List[LTerm]()) <|>        
                    // at least arity 1
                    (
                        lterm <~>
                        many(spaces ~> ',' ~> spaces ~> lterm <~ spaces) <~ ')'
                    ).map { (c: LTerm, cs: List[LTerm]) => c :: cs }
                )
            )
        ).map { (res: (List[Char], List[LTerm])) =>
            LTerm.FuncAp(
                Function(res._1.mkString, res._2.length), res._2
            )
        }

    // funcAp needs to have a higher precedence to work properly
    lazy val lterm = atomic(funcAp) <|> variable

    def parse(input: String) = ???
}
