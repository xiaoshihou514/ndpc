package ndpc

import ndpc.FormulaParser._
import ndpc.syntax.Formula._

@inline implicit def stringLift(str: String): LTerm.Variable =
    LTerm.Variable(str)
def F = Function

class ParserSpec extends UnitSpec {
    "spaces" should "match zero or more spaces" in {
        assert(spc.parse("").isSuccess)
        assert(spc.parse(" ").isSuccess)
        assert(spc.parse("    ").isSuccess)
    }

    "A variable" should "be any non keyword string" in {
        assert(variable.parse("valid").get === LTerm.Variable("valid"))
        assert(variable.parse("abc ").get === LTerm.Variable("abc"))
    }

    "A function application" should "be a function followed by (, some lterms and a )" in {
        val example = LTerm.FuncAp(
          Function("foo", 3),
          List("x", "y", "z")
        )
        assert(funcAp.parse("foo(x, y, z)").get === example)
        assert(funcAp.parse("foo  (x , y,z  )").get === example)
        val nested1 = LTerm.FuncAp(
          Function("bar", 1),
          List(example)
        )
        val nested2 = LTerm.FuncAp(
          Function("bar", 2),
          List(example, ("kk"))
        )
        val nested3 = LTerm.FuncAp(
          Function("baz", 6),
          List(nested2, "h", "j", "k", "l", nested1)
        )
        assert(funcAp.parse("bar (foo(x,y,z))").get === nested1)
        assert(funcAp.parse("bar (foo(x,y,z)    , kk)").get === nested2)
        assert(
          funcAp
              .parse(
                "baz (bar (foo(x,y,z)    , kk), h, j  ,  k,l, bar (foo(x,y  ,z)))"
              )
              .get === nested3
        )
    }

    "An LTerm" should "be a funcap or a variable" in {
        assert(lterm.parse("par").get === LTerm.Variable("par"))
        assert(lterm.parse("sley").get === LTerm.Variable("sley"))
        val example = LTerm.FuncAp(
          Function("foo", 3),
          List("x", "y", "z")
        )
        assert(lterm.parse("foo(x, y, z)").get === example)
        assert(lterm.parse("foo  (x , y,z  )").get === example)
        val nested1 = LTerm.FuncAp(
          Function("bar", 1),
          List(example)
        )
        val nested2 = LTerm.FuncAp(
          Function("bar", 2),
          List(example, ("kk"))
        )
        val nested3 = LTerm.FuncAp(
          Function("baz", 6),
          List(nested2, "h", "j", "k", "l", nested1)
        )
        assert(lterm.parse("bar (foo(x,y,z))").get === nested1)
        assert(lterm.parse("bar (foo(x,y,z)    , kk)").get === nested2)
        assert(
          lterm
              .parse(
                "baz (bar (foo(x,y,z)    , kk), h, j  ,  k,l, bar (foo(x,y  ,z)))"
              )
              .get === nested3
        )
    }

    "A predAp" should "be a predicate applied to multiple lterms" in {
        val example1 = LFormula.PredAp(
          Predicate("foo", 3),
          List("x", "y", "z")
        )
        assert(predAp.parse("foo ( x, y, z )").get === example1)
        val example2 = LFormula.PredAp(
          Predicate("foo", 2),
          List(
            LTerm.FuncAp(
              Function("bar", 2),
              List("ss", "l")
            ),
            "w"
          )
        )
        assert(predAp.parse("foo( bar(ss,l), w)").get === example2)
        val example3 = LFormula.PredAp(
          Predicate("foo", 2),
          List(
            LTerm.FuncAp(
              Function("bar", 2),
              List(
                "ss",
                LTerm.FuncAp(
                  Function("wacc", 4),
                  List("w", "a", "c", "c")
                )
              )
            ),
            "w"
          )
        )
        assert(
          predAp
              .parse("foo( bar(ss,wacc   (w  , a , c,c)  ), w)")
              .get === example3
        )
    }
}
