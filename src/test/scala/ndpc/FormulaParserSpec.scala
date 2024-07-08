package ndpc

import ndpc.FormulaParser._
import ndpc.syntax.Formula._

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
          List(
            LTerm.Variable("x"),
            LTerm.Variable("y"),
            LTerm.Variable("z")
          )
        )
        assert(funcAp.parse("foo(x, y, z)").get === example)
        assert(funcAp.parse("foo  (x , y,z  )").get === example)
        val nested1 = LTerm.FuncAp(
          Function("bar", 1),
          List(example)
        )
        val nested2 = LTerm.FuncAp(
          Function("bar", 2),
          List(example, LTerm.Variable("kk"))
        )
        val nested3 = LTerm.FuncAp(
          Function("baz", 6),
          List(
            nested2,
            LTerm.Variable("h"),
            LTerm.Variable("j"),
            LTerm.Variable("k"),
            LTerm.Variable("l"),
            nested1
          )
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
          List(
            LTerm.Variable("x"),
            LTerm.Variable("y"),
            LTerm.Variable("z")
          )
        )
        assert(lterm.parse("foo(x, y, z)").get === example)
        assert(lterm.parse("foo  (x , y,z  )").get === example)
        val nested1 = LTerm.FuncAp(
          Function("bar", 1),
          List(example)
        )
        val nested2 = LTerm.FuncAp(
          Function("bar", 2),
          List(example, LTerm.Variable("kk"))
        )
        val nested3 = LTerm.FuncAp(
          Function("baz", 6),
          List(
            nested2,
            LTerm.Variable("h"),
            LTerm.Variable("j"),
            LTerm.Variable("k"),
            LTerm.Variable("l"),
            nested1
          )
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
}
