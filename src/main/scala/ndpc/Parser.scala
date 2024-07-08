package ndpc

import parsley.Parsley, Parsley.{many, some, atomic}
import parsley.character.{satisfy, char}
import parsley.syntax.character.{charLift, stringLift}
import parsley.debug._

import ndpc.syntax.Formula._

object FormulaParser {
    val spc = many(' ')
    // LTerm
    val keywords = Set('(', ')', ' ', '.', ',')
    val ident = some(satisfy(!keywords.contains(_)))
    val variable = ident.map { (cs: List[Char]) => LTerm.Variable(cs.mkString) }
    lazy val funcAp: Parsley[LTerm.FuncAp] =
        (
          ident <~ spc <~> (
            '(' ~> spc ~>
                (
                  // 0 arity
                  ')'.map(_ => List[LTerm]()) <|>
                      // at least arity 1
                      (
                        lterm <~>
                            many(spc ~> ',' ~> spc ~> lterm <~ spc) <~ ')'
                      ).map { (c: LTerm, cs: List[LTerm]) => c :: cs }
                )
          )
        ).map { (res: (List[Char], List[LTerm])) =>
            LTerm.FuncAp(
              Function(res._1.mkString, res._2.length),
              res._2
            )
        }

    // funcAp needs to have a higher precedence to work properly
    lazy val lterm = atomic(funcAp) <|> variable
}

object Parser {
    def parse(input: String) = ???
}
