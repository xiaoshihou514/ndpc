package ndpc

import ndpc.parsers.RuleParser._
import ndpc.expr.Rule._

class RuleParserSpec extends UnitSpec {
    "A rule" should "be defined in Rule.scala" in {
        // Special rules
        assert(rule.parse("LEM").get === LEM())
        assert(rule.parse("MT(1 ,1   )").get === MT(1, 1))
        assert(rule.parse("PC  (4,5)").get === PC(4, 5))
        assert(rule.parse("refl").get === Refl())
        assert(rule.parse("=sub (    1, 4)").get === EqSub(1, 4))
        assert(rule.parse("sym    (1  )").get === Sym(1))
        assert(rule.parse("given").get === Given())
        assert(rule.parse("premise").get === Premise())
        assert(rule.parse("ass").get === Ass())
        assert(rule.parse("tick ( 9)").get === Tick(9))
        assert(rule.parse("forall   I const").get === ForallIConst())

        // Introductions
        assert(rule.parse("^I (1,9)").get === AndIntro(1, 9))
        assert(rule.parse("->I ( 8,10)").get === ImpliesIntro(8, 10))
        assert(rule.parse("/I  (5999)").get === OrIntro(5999))
        assert(rule.parse("~I  (1888, 6)").get === NotIntro(1888, 6))
        assert(rule.parse("~~I  (1918)").get === DoubleNegIntro(1918))
        assert(rule.parse("~~E  (1918)").get === DoubleNegElim(1918))
        assert(rule.parse("FI  (1991, 2399)").get === FalsityIntro(1991, 2399))
        assert(rule.parse("TI").get === TruthIntro())
        assert(rule.parse("<->I  (99, 66)").get === EquivIntro(99, 66))
        assert(rule.parse("existsI(3)").get === ExistsIntro(3))
        assert(rule.parse("forallI(4 ,2 )").get === ForallIntro(4, 2))

        // Elimination
        assert(rule.parse("^E(7)").get === AndElim(7))
        assert(rule.parse("->E(3,8)").get === ImpliesElim(3, 8))
        assert(rule.parse("/E ( 7, 11 )").get === OrElim(7, 11))
        assert(rule.parse("~E (4, 5)").get === NotElim(4, 5))
        assert(rule.parse("~~E ( 1 )").get === DoubleNegElim(1))
        assert(rule.parse("FE(7)").get === FalsityElim(7))
        assert(rule.parse("<->E (4,5)").get === EquivElim(4, 5))
        assert(rule.parse("existsE(1,2)").get === ExistsElim(1, 2))
        assert(rule.parse("forallE (77)").get === ForallElim(77))
    }
}
