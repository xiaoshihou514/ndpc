package ndpc

import ndpc.parsers.FormulaParser._
import ndpc.expr.Formula._
import scala.language.implicitConversions // yes, I know what I am doing

given Conversion[String, PredAp] with
    def apply(s: String): PredAp = PredAp(s, Nil)

class FormulaParserSpec extends UnitSpec {
    "A predAp" should "be a predicate applied to multiple lterms" in {
        val example1 = PredAp("foo", List("x", "y", "z"))
        assert(predAp.parse("foo ( x, y, z )").get == example1)
        assert(lformula.parse("foo ( x, y, z )").get == example1)
        val example2 = PredAp(
          "foo",
          List(
            PredAp("bar", List("ss", "l")),
            "w"
          )
        )
        assert(predAp.parse("foo( bar(ss,l), w)").get == example2)
        assert(lformula.parse("foo( bar(ss,l), w)").get == example2)
        val example3 = PredAp(
          "foo",
          List(
            PredAp(
              "bar",
              List(
                "ss",
                PredAp("wacc", List("w", "a", "c", "c"))
              )
            ),
            "w"
          )
        )
        assert(
          predAp
              .parse("foo( bar(ss,wacc   (w  , a , c,c)  ), w)")
              .get == example3
        )
        assert(
          lformula
              .parse("foo( bar(ss,wacc   (w  , a , c,c)  ), w)")
              .get == example3
        )
        val sugar = PredAp("foo", Nil)
        assert(predAp.parse("foo^bar").get == sugar)
    }

    "eq" should "be an predAp = an predAp" in {
        val example1 = Eq("a", "bb")
        assert(equ.parse("a   = bb").get == example1)
        assert(lformula.parse("a   = bb").get == example1)
        val example2 = Eq(
          "x",
          PredAp(
            "wuu",
            List("a", "wa")
          )
        )
        assert(equ.parse("x=  wuu (  a, wa)").get == example2)
        assert(lformula.parse("x=  wuu (  a, wa)").get == example2)
        val example3 = Eq(
          PredAp("jkjk", Nil),
          PredAp(
            "u",
            List(
              PredAp("qo", List("j", "w"))
            )
          )
        )
        assert(equ.parse("jkjk ()= u(qo(j,w))").get == example3)
        assert(lformula.parse("jkjk ()= u(qo(j,w))").get == example3)
    }

    "Truth and falsity" should "be a single char T/F" in {
        assert(truth.parse("T ^ fff").get == Truth())
        assert(truth.parse("T").get == Truth())
        assert(truth.parse("TasVar").isFailure)
        assert(falsity.parse("Fstart").isFailure)
    }

    "An atom in a LFormula" should "be a predAp / T / F" in {
        val atom1 = PredAp(
          "TStartFunc",
          List(
            PredAp(
              "FStartFunc",
              List("a", "b")
            ),
            PredAp("fs", Nil),
            "j"
          )
        )
        assert(
          atom.parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j)").get == atom1
        )
    }

    "An lformula" should "be connectives + lfromula / forall(lformula) / exists(lformula)" in {
        val atomWithBrackets = PredAp(
          "TStartFunc",
          List(
            PredAp(
              "FStartFunc",
              List("a", "b")
            ),
            PredAp("fs", Nil),
            "j"
          )
        )
        assert(
          lformula
              .parse("(  TStartFunc (FStartFunc(a,b)  ,   fs(), j))")
              .get == atomWithBrackets
        )

        val connectives_1 = Not(atomWithBrackets)
        assert(
          lformula
              .parse("~ (  TStartFunc (FStartFunc(a,b)  ,   fs(), j))")
              .get == connectives_1
        )
        assert(
          lformula
              .parse("~(  TStartFunc (FStartFunc(a,b)  ,   fs(), j))")
              .get == connectives_1
        )

        val connectives_2 = And(
          atomWithBrackets,
          Truth()
        )
        assert(
          lformula
              .parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j) ^T")
              .get == connectives_2
        )
        assert(
          lformula
              .parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j) ^  T")
              .get == connectives_2
        )
        assert(
          lformula
              .parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j)^  T")
              .get == connectives_2
        )

        val connectives_3 = Or(
          Falsity(),
          atomWithBrackets
        )
        assert(
          lformula
              .parse("F/TStartFunc (FStartFunc(a,b)  ,   fs(), j)")
              .get == connectives_3
        )

        val connectives_4 = Implies(
          atomWithBrackets,
          atomWithBrackets
        )
        assert(
          lformula
              .parse(
                "(TStartFunc(FStartFunc(a,b),fs(),j)) ->  TStartFunc (FStartFunc(a,b)  ,   fs(), j)"
              )
              .get == connectives_4
        )

        val connectives_5 = Equiv(
          Truth(),
          Falsity()
        )
        assert(lformula.parse("(((((T)))))    <->  (F)").get == connectives_5)

        val connectives_6 = And(
          Implies(
            PredAp("p", Nil),
            PredAp("q", Nil)
          ),
          Implies(
            Not(PredAp("p", Nil)),
            PredAp("r", Nil)
          )
        )
        assert(lformula.parse("(p -> q) ^ (~p -> r)").get == connectives_6)

        val connectives_7 = And(
          Eq("p", "q"),
          Implies(
            Not(PredAp("p", Nil)),
            PredAp("r", Nil)
          )
        )
        assert(
          lformula.parse("(p=    q   )^    ( ~  p-> r  )").get == connectives_7
        )

        val forall = Forall(
          "q",
          connectives_7
        )
        assert(
          lformula
              .parse("forall q. ((p=    q   )^    ( ~  p-> r  ))")
              .get == forall
        )

        val exists = Forall(
          "𝝓",
          Exists(
            "A",
            Or(
              PredAp(
                "foo",
                List("𝝓", "φ", "A")
              ),
              Exists(
                "B",
                PredAp(
                  "bar",
                  List("𝝓", "B")
                )
              )
            )
          )
        )
        assert(
          lformula
              .parse(
                "forall 𝝓 . (exists A. ( foo(𝝓 ,φ ,A) / (exists B. (bar(𝝓, B)))))"
              )
              .get == exists
        )

        val precedence = Implies(
          And(
            PredAp("a", Nil),
            PredAp("b", Nil)
          ),
          Equiv(
            PredAp("a", Nil),
            PredAp("a", Nil)
          )
        )
        assert(lformula.parse("a^b->(a<->a)").get == precedence)

    }

    "Any LFormula" should "be parsed correctly" in {
        val `all green dragons can fly` = Forall(
          "x",
          Implies(
            And(
              PredAp("dragon", List("x")),
              PredAp("green", List("x"))
            ),
            PredAp("fly", List("x"))
          )
        )
        assert(
          lformula
              .parse("forall x. (dragon(x) ^ green(x) -> fly(x))")
              .get == `all green dragons can fly`
        )
    }

    "An LFormula" should "respect braces" in {
        val precedence =
            And(
              PredAp("a", Nil),
              Implies(
                PredAp("a", Nil),
                PredAp("b", Nil)
              )
            )
        assert(
          lformula.parse("a ^(a ->b)").get == precedence
        )
    }
}
