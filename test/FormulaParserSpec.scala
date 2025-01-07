package ndpc

import ndpc.parsers.FormulaParser._
import ndpc.expr.Formula._

def p(s: String) = PredAp(s, Nil)

class FormulaParserSpec extends UnitSpec {
    "A predAp" should "be a predicate applied to multiple lterms" in {
        val example1 = PredAp("foo", List(p("x"), p("y"), p("z")))
        predAp.parse("foo ( x, y, z )").get shouldBe example1
        lformula.parse("foo ( x, y, z )").get shouldBe example1
        val example2 = PredAp(
          "foo",
          List(
            PredAp("bar", List(p("ss"), p("l"))),
            p("w")
          )
        )
        predAp.parse("foo( bar(ss,l), w)").get shouldBe example2
        lformula.parse("foo( bar(ss,l), w)").get shouldBe example2
        val example3 = PredAp(
          "foo",
          List(
            PredAp(
              "bar",
              List(
                p("ss"),
                PredAp("wacc", List(p("w"), p("a"), p("c"), p("c")))
              )
            ),
            p("w")
          )
        )
        predAp.parse("foo( bar(ss,wacc   (w  , a , c,c)  ), w)").get shouldBe example3
        lformula.parse("foo( bar(ss,wacc   (w  , a , c,c)  ), w)").get shouldBe example3
        val sugar = PredAp("foo", Nil)
        predAp.parse("foo^bar").get shouldBe sugar
    }

    "eq" should "be an predAp = an predAp" in {
        val example1 = Eq(p("a"), p("bb"))
        equ.parse("a   = bb").get shouldBe example1
        lformula.parse("a   = bb").get shouldBe example1
        val example2 = Eq(
          p("x"),
          PredAp(
            "wuu",
            List(p("a"), p("wa"))
          )
        )
        equ.parse("x=  wuu (  a, wa)").get shouldBe example2
        lformula.parse("x=  wuu (  a, wa)").get shouldBe example2
        val example3 = Eq(
          PredAp("jkjk", Nil),
          PredAp(
            "u",
            List(
              PredAp("qo", List(p("j"), p("w")))
            )
          )
        )
        equ.parse("jkjk ()= u(qo(j,w))").get shouldBe example3
        lformula.parse("jkjk ()= u(qo(j,w))").get shouldBe example3
    }

    "Truth and falsity" should "be a single char T/F" in {
        truth.parse("T ^ fff").get shouldBe Truth
        truth.parse("T").get shouldBe Truth

        assert(truth.parse("TasVar").isFailure)
        assert(falsity.parse("Fstart").isFailure)
    }

    "An atom in a LFormula" should "be a predAp / T / F" in {
        val atom1 = PredAp(
          "TStartFunc",
          List(
            PredAp(
              "FStartFunc",
              List(p("a"), p("b"))
            ),
            PredAp("fs", Nil),
            p("j")
          )
        )
        atom.parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j)").get shouldBe atom1
    }

    "An lformula" should "be connectives + lfromula / forall(lformula) / exists(lformula)" in {
        val atomWithBrackets = PredAp(
          "TStartFunc",
          List(
            PredAp(
              "FStartFunc",
              List(p("a"), p("b"))
            ),
            PredAp("fs", Nil),
            p("j")
          )
        )
        lformula
            .parse("(  TStartFunc (FStartFunc(a,b)  ,   fs(), j))")
            .get shouldBe atomWithBrackets

        val connectives_1 = Not(atomWithBrackets)
        lformula.parse("~ (  TStartFunc (FStartFunc(a,b)  ,   fs(), j))").get shouldBe connectives_1
        lformula.parse("~(  TStartFunc (FStartFunc(a,b)  ,   fs(), j))").get shouldBe connectives_1

        val connectives_2 = And(
          atomWithBrackets,
          Truth
        )
        lformula.parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j) ^T").get shouldBe connectives_2
        lformula.parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j) ^  T").get shouldBe connectives_2
        lformula.parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j)^  T").get shouldBe connectives_2

        val connectives_3 = Or(
          Falsity,
          atomWithBrackets
        )
        lformula.parse("F/TStartFunc (FStartFunc(a,b)  ,   fs(), j)").get shouldBe connectives_3

        val connectives_4 = Implies(
          atomWithBrackets,
          atomWithBrackets
        )
        lformula
            .parse(
              "(TStartFunc(FStartFunc(a,b),fs(),j)) ->  TStartFunc (FStartFunc(a,b)  ,   fs(), j)"
            )
            .get shouldBe connectives_4

        val connectives_5 = Equiv(
          Truth,
          Falsity
        )
        lformula.parse("(((((T)))))    <->  (F)").get shouldBe connectives_5

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
        lformula.parse("(p -> q) ^ (~p -> r)").get shouldBe connectives_6

        val connectives_7 = And(
          Eq(p("p"), p("q")),
          Implies(
            Not(PredAp("p", Nil)),
            PredAp("r", Nil)
          )
        )
        lformula.parse("(p=    q   )^    ( ~  p-> r  )").get shouldBe connectives_7

        val forall = Forall(
          "q",
          connectives_7
        )
        lformula.parse("forall q. ((p=    q   )^    ( ~  p-> r  ))").get shouldBe forall

        val exists = Forall(
          "𝝓",
          Exists(
            "A",
            Or(
              PredAp(
                "foo",
                List(p("𝝓"), p("φ"), p("A"))
              ),
              Exists(
                "B",
                PredAp(
                  "bar",
                  List(p("𝝓"), p("B"))
                )
              )
            )
          )
        )
        lformula
            .parse("forall 𝝓 . (exists A. ( foo(𝝓 ,φ ,A) / (exists B. (bar(𝝓, B)))))")
            .get shouldBe exists

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
        lformula.parse("a^b->(a<->a)").get shouldBe precedence

    }

    "Any LFormula" should "be parsed correctly" in {
        val `all green dragons can fly` = Forall(
          "x",
          Implies(
            And(
              PredAp("dragon", List(p("x"))),
              PredAp("green", List(p("x")))
            ),
            PredAp("fly", List(p("x")))
          )
        )
        lformula
            .parse("forall x. (dragon(x) ^ green(x) -> fly(x))")
            .get shouldBe `all green dragons can fly`
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
        lformula.parse("a ^(a ->b)").get shouldBe precedence
    }
}
