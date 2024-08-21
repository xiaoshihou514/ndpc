package ndpc

import ndpc.parsers.RuleParser._
import ndpc.expr.Rule._

class RuleParserSpec extends UnitSpec {
    "A rule" should "be defined in Rule.scala" in {
        // Derived rules
        assert(rule.parse("LEM").get === Rule.Builtin(Special.LEM()))
        assert(rule.parse("MT(1 ,1   )").get === Rule.Builtin(Special.MT(1, 1)))
        assert(rule.parse("PC  (4,5)").get === Rule.Builtin(Special.PC(4, 5)))
        assert(rule.parse("refl").get === Rule.Builtin(Special.Refl()))
        assert(
          rule.parse("=sub (    1, 4)").get === Rule.Builtin(
            Special.EqSub(1, 4)
          )
        )
        assert(rule.parse("sym    (1  )").get === Rule.Builtin(Special.Sym(1)))
        assert(rule.parse("given").get === Rule.Builtin(Special.Given()))
        assert(rule.parse("premise").get === Rule.Builtin(Special.Premise()))
        assert(rule.parse("ass").get === Rule.Builtin(Special.Ass()))
        assert(rule.parse("tick ( 9)").get === Rule.Builtin(Special.Tick(9)))
        assert(
          rule.parse("forall   I const").get === Rule.Builtin(
            Special.ForallIConst()
          )
        )

        // Introductions
        assert(
          rule.parse("^I (1,9)").get === Rule.Intro(Introduction.And(1, 9))
        )
        assert(
          rule.parse("->I ( 8,10)").get === Rule.Intro(
            Introduction.Implies(8, 10)
          )
        )
        assert(
          rule.parse("/I  (5999)").get === Rule.Intro(
            Introduction.Or(5999)
          )
        )
        assert(
          rule.parse("~I  (1888, 6)").get === Rule.Intro(
            Introduction.Not(1888, 6)
          )
        )
        assert(
          rule.parse("~~I  (1918)").get === Rule.Intro(
            Introduction.DoubleNeg(1918)
          )
        )
        assert(
          rule.parse("FI  (1991, 2399)").get === Rule.Intro(
            Introduction.Falsity(1991, 2399)
          )
        )
        assert(rule.parse("TI").get === Rule.Intro(Introduction.Truth()))
        assert(
          rule.parse("<->I  (99, 66)").get === Rule.Intro(
            Introduction.Equiv(99, 66)
          )
        )
        assert(
          rule.parse("existsI(3)").get === Rule.Intro(
            Introduction.Exists(3)
          )
        )
        assert(
          rule.parse("forallI(4 ,2 )").get === Rule.Intro(
            Introduction.Forall(4, 2)
          )
        )

        // Elimination
        assert(
          rule.parse("^E(7)").get === Rule.Elim(Elimination.And(7))
        )
        assert(
          rule.parse("->E(3,8)").get === Rule.Elim(Elimination.Implies(3, 8))
        )
        assert(
          rule.parse("/E ( 7, 11 )").get === Rule.Elim(Elimination.Or(7, 11))
        )
        assert(
          rule.parse("~E (4, 5)").get === Rule.Elim(Elimination.Not(4, 5))
        )
        assert(
          rule.parse("~~E ( 1 )").get === Rule.Elim(Elimination.DoubleNeg(1))
        )
        assert(
          rule.parse("FE(7,1)").get === Rule.Elim(Elimination.Falsity(7, 1))
        )
        assert(
          rule.parse("<->E (4,5)").get === Rule.Elim(Elimination.Equiv(4, 5))
        )
        assert(
          rule.parse("existsE(1,2)").get === Rule.Elim(Elimination.Exists(1, 2))
        )
        assert(
          rule.parse("forallE (77)").get === Rule.Elim(Elimination.Forall(77))
        )
    }
}
