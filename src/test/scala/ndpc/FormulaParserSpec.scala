package ndpc

import ndpc.FormulaParser._
import ndpc.Formula._

// just to save some typing
given Conversion[String, LTerm.Variable] with
    def apply(s: String): LTerm.Variable = LTerm.Variable(s)
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
          F("foo", 3),
          List("x", "y", "z")
        )
        assert(funcAp.parse("foo(x, y, z)").get === example)
        assert(funcAp.parse("foo  (x , y,z  )").get === example)
        val nested1 = LTerm.FuncAp(
          F("bar", 1),
          List(example)
        )
        val nested2 = LTerm.FuncAp(
          F("bar", 2),
          List(example, ("kk"))
        )
        val nested3 = LTerm.FuncAp(
          F("baz", 6),
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
          F("foo", 3),
          List("x", "y", "z")
        )
        assert(lterm.parse("foo(x, y, z)").get === example)
        assert(lterm.parse("foo  (x , y,z  )").get === example)
        val nested1 = LTerm.FuncAp(
          F("bar", 1),
          List(example)
        )
        val nested2 = LTerm.FuncAp(
          F("bar", 2),
          List(example, ("kk"))
        )
        val nested3 = LTerm.FuncAp(
          F("baz", 6),
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
        // assert(lformula.parse("foo ( x, y, z )").get === example1)
        val example2 = LFormula.PredAp(
          Predicate("foo", 2),
          List(
            LTerm.FuncAp(
              F("bar", 2),
              List("ss", "l")
            ),
            "w"
          )
        )
        assert(predAp.parse("foo( bar(ss,l), w)").get === example2)
        // assert(lformula.parse("foo( bar(ss,l), w)").get === example2)
        val example3 = LFormula.PredAp(
          Predicate("foo", 2),
          List(
            LTerm.FuncAp(
              F("bar", 2),
              List(
                "ss",
                LTerm.FuncAp(
                  F("wacc", 4),
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
        // assert(
        //   lformula
        //       .parse("foo( bar(ss,wacc   (w  , a , c,c)  ), w)")
        //       .get === example3
        // )
        val sugar = LFormula.PredAp(
          Predicate("foo", 0),
          List()
        )
        assert(predAp.parse("foo^bar").get === sugar)
        // assert(lformula.parse("foo^bar").get === sugar)
    }

    "eq" should "be an lterm = an lterm" in {
        val example1 = LFormula.Eq("a", "bb")
        assert(equ.parse("a   = bb").get === example1)
        // assert(lformula.parse("a   = bb").get === example1)
        val example2 = LFormula.Eq(
          "x",
          LTerm.FuncAp(
            F("wuu", 2),
            List("a", "wa")
          )
        )
        assert(equ.parse("x=  wuu (  a, wa)").get === example2)
        // assert(lformula.parse("x=  wuu (  a, wa)").get === example2)
        val example3 = LFormula.Eq(
          LTerm.FuncAp(
            F("jkjk", 0),
            List()
          ),
          LTerm.FuncAp(
            F("u", 1),
            List(
              LTerm.FuncAp(
                F("qo", 2),
                List("j", "w")
              )
            )
          )
        )
        assert(equ.parse("jkjk ()= u(qo(j,w))").get === example3)
        // assert(lformula.parse("jkjk ()= u(qo(j,w))").get === example3)
    }

    "Truth and falsity" should "be a single char T/F" in {
        assert(truth.parse("T ^").get === LFormula.Truth)
        assert(falsity.parse("F(some reason)").get === LFormula.Falsity)
        assert(truth.parse("TasVar").isFailure)
        assert(falsity.parse("Fstart").isFailure)
    }

    "Logical connectives" should "be ~p, p^q, p/q, p->q, p<->q" in {
        // TODO
    }
}
