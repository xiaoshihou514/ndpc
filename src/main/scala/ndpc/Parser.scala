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
    def args[A](one: Parsley[A], sep: Char): Parsley[List[A]] =
        '(' ~> spc ~>
            // 0 arity
            (')'.map(_ => List[A]()) <|>
                // 1+ arity
                (one <~> many(spc ~> ',' ~> spc ~> one <~ spc) <~ ')').map {
                    (c: A, cs: List[A]) => c :: cs
                })

    val variable = ident.map { (cs: List[Char]) => LTerm.Variable(cs.mkString) }
    lazy val funcAp: Parsley[LTerm.FuncAp] =
        (
          ident <~ spc <~> args(lterm, ',')
        ).map { (res: (List[Char], List[LTerm])) =>
            LTerm.FuncAp(
              Function(res._1.mkString, res._2.length),
              res._2
            )
        }

    // funcAp needs to have a higher precedence to work properly
    lazy val lterm = atomic(funcAp) <|> variable

    LFormula
    lazy val predAp: Parsley[LFormula.PredAp] =
        ((ident <~ spc).map(_.mkString) <~> args(lterm, ','))
            .map { (res: (String, List[LTerm])) =>
                LFormula.PredAp(
                  Predicate(res._1, res._2.length),
                  res._2
                )
            }
    val eq =
        (lterm <~> (spc ~> '=' ~> spc) ~> lterm).map { (res: (LTerm, LTerm)) =>
            LFormula.Eq(res._1, res._2)
        }
    val truth = 'T'
    val falsity = 'F'
    lazy val not = ('~' ~> spc ~> lformula).map(LFormula.Not.apply)
    lazy val and = (lformula <~> (spc ~> '^' ~> spc ~> lformula))
        .map { (res: (LFormula[_], LFormula[_])) =>
            LFormula.And(res._1, res._2)
        }
    lazy val or = (lformula <~> (spc ~> '/' ~> spc ~> lformula))
        .map { (res: (LFormula[_], LFormula[_])) =>
            LFormula.Or(res._1, res._2)
        }
    lazy val implies = (lformula <~> (spc ~> "->" ~> spc ~> lformula))
        .map { (res: (LFormula[_], LFormula[_])) =>
            LFormula.Implies(res._1, res._2)
        }
    lazy val equiv = (lformula <~> (spc ~> "<->" ~> spc ~> lformula))
        .map { (res: (LFormula[_], LFormula[_])) =>
            LFormula.Equiv(res._1, res._2)
        }
    lazy val forall = ???
    lazy val lformula: Parsley[LFormula[_]] = ???
}

object Parser {
    def parse(input: String) = ???
}
