package ndpc

import ndpc.frontend.parsers.RuleParser._
import ndpc.frontend.expr.rule._

class RuleParserSpec extends UnitSpec {
    "A rule" should "be defined in Rule.scala" in {
        // Special rules
        rule.parse("LEM").get shouldBe LEM
        rule.parse("MT(1 ,1   )").get shouldBe MT(1, 1)
        rule.parse("PC  (4,5)").get shouldBe PC(4, 5)
        rule.parse("refl").get shouldBe Refl
        rule.parse("=sub (    1, 4)").get shouldBe EqSub(1, 4)
        rule.parse("sym    (1  )").get shouldBe Sym(1)
        rule.parse("given").get shouldBe Given
        rule.parse("premise").get shouldBe Premise
        rule.parse("ass").get shouldBe Ass
        rule.parse("tick ( 9)").get shouldBe Tick(9)
        rule.parse("forall   I const").get shouldBe ForallIConst

        // Introductions
        rule.parse("^I (1,9)").get shouldBe AndIntro(1, 9)
        rule.parse("->I ( 8,10)").get shouldBe ImpliesIntro(8, 10)
        rule.parse("/I  (5999)").get shouldBe OrIntro(5999)
        rule.parse("~I  (1888, 6)").get shouldBe NotIntro(1888, 6)
        rule.parse("~~I  (1918)").get shouldBe DoubleNegIntro(1918)
        rule.parse("~~E  (1918)").get shouldBe DoubleNegElim(1918)
        rule.parse("FI  (1991, 2399)").get shouldBe FalsityIntro(1991, 2399)
        rule.parse("TI").get shouldBe TruthIntro
        rule.parse("<->I  (99, 66)").get shouldBe EquivIntro(99, 66)
        rule.parse("existsI(3)").get shouldBe ExistsIntro(3)
        rule.parse("forallI(4 ,2 )").get shouldBe ForallIntro(4, 2)

        // Elimination
        rule.parse("^E(7)").get shouldBe AndElim(7)
        rule.parse("->E(3,8)").get shouldBe ImpliesElim(3, 8)
        rule.parse("/E (1, 7, 11,12,13 )").get shouldBe OrElim(1, 7, 11, 12, 13)
        rule.parse("~E (4, 5)").get shouldBe NotElim(4, 5)
        rule.parse("~~E ( 1 )").get shouldBe DoubleNegElim(1)
        rule.parse("FE(7)").get shouldBe FalsityElim(7)
        rule.parse("<->E (4,5)").get shouldBe EquivElim(4, 5)
        rule.parse("existsE(1,2,3)").get shouldBe ExistsElim(1, 2, 3)
        rule.parse("forallE (77)").get shouldBe ForallElim(77)
    }
}
