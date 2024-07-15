package ndpc

import parsley.Parsley
import parsley.syntax.character.{stringLift, charLift}
import Parsley.some
import parsley.character.{digit}
import Parsley.many


//util
// parses address of rule into a list of int
val adr: Parsley[List[Int]] = '(' ~> some(digit).map(_.mkString.toInt) 
                              <::> many(','~>some(digit).map(_.mkString.toInt)) <~ ')'
