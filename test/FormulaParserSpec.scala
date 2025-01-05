package ndpc

import ndpc.parsers.FormulaParser._
import ndpc.expr.Formula._
import scala.language.implicitConversions // yes, I know what I am doing

given Conversion[String, PredAp] with
    def apply(s: String): PredAp = PredAp(s, Nil)

class FormulaParserSpec extends UnitSpec {
    "A predAp" should "be a predicate applied to multiple lterms" in {
        val example1 = PredAp("foo", List("x", "y", "z"))
        predAp.parse("foo ( x, y, z )").get shouldBe example1
        lformula.parse("foo ( x, y, z )").get shouldBe example1
        val example2 = PredAp(
          "foo",
          List(
            PredAp("bar", List("ss", "l")),
            "w"
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
                "ss",
                PredAp("wacc", List("w", "a", "c", "c"))
              )
            ),
            "w"
          )
        )
        predAp.parse("foo( bar(ss,wacc   (w  , a , c,c)  ), w)").get shouldBe example3
        lformula.parse("foo( bar(ss,wacc   (w  , a , c,c)  ), w)").get shouldBe example3
        val sugar = PredAp("foo", Nil)
        predAp.parse("foo^bar").get shouldBe sugar
    }

    "eq" should "be an predAp = an predAp" in {
        val example1 = Eq("a", "bb")
        equ.parse("a   = bb").get shouldBe example1
        lformula.parse("a   = bb").get shouldBe example1
        val example2 = Eq(
          "x",
          PredAp(
            "wuu",
            List("a", "wa")
          )
        )
        equ.parse("x=  wuu (  a, wa)").get shouldBe example2
        lformula.parse("x=  wuu (  a, wa)").get shouldBe example2
        val example3 = Eq(
          PredAp("jkjk", Nil),
          PredAp(
            "u",
            List(
              PredAp("qo", List("j", "w"))
            )
          )
        )
        equ.parse("jkjk ()= u(qo(j,w))").get shouldBe example3
        lformula.parse("jkjk ()= u(qo(j,w))").get shouldBe example3
    }

    "Truth and falsity" should "be a single char T/F" in {
        truth.parse("T ^ fff").get shouldBe Truth()
        truth.parse("T").get shouldBe Truth()

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
        atom.parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j)").get shouldBe atom1
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
        lformula
            .parse("(  TStartFunc (FStartFunc(a,b)  ,   fs(), j))")
            .get shouldBe atomWithBrackets

        val connectives_1 = Not(atomWithBrackets)
        lformula.parse("~ (  TStartFunc (FStartFunc(a,b)  ,   fs(), j))").get shouldBe connectives_1
        lformula.parse("~(  TStartFunc (FStartFunc(a,b)  ,   fs(), j))").get shouldBe connectives_1

        val connectives_2 = And(
          atomWithBrackets,
          Truth()
        )
        lformula.parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j) ^T").get shouldBe connectives_2
        lformula.parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j) ^  T").get shouldBe connectives_2
        lformula.parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j)^  T").get shouldBe connectives_2

        val connectives_3 = Or(
          Falsity(),
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
          Truth(),
          Falsity()
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
          Eq("p", "q"),
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
              PredAp("dragon", List("x")),
              PredAp("green", List("x"))
            ),
            PredAp("fly", List("x"))
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
