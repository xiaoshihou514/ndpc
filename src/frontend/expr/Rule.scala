package ndpc.frontend.expr

import ndpc.frontend.expr.formula._

import parsley.generic.*

object rule {
    sealed trait Rule

    // ∧-introduction, ∧I: you have to have already introduced both sides
    case class AndIntro(left: Int, right: Int) extends Rule
    // →-introduction, →I: you assume 𝝓 and prove φ
    case class ImpliesIntro(ass: Int, res: Int) extends Rule
    // ∨-introduction, ∨I: prove either side
    case class OrIntro(either: Int) extends Rule
    // ¬-introduction, ¬I: assume 𝝓 and get ⊥
    case class NotIntro(orig: Int, bottom: Int) extends Rule
    // ¬¬introduction, ¬¬I: From 𝝓, deduce ¬¬𝝓
    // 1 𝝓      proved this somehow
    // 2 ¬¬𝝓    ¬¬I(1)
    case class DoubleNegIntro(orig: Int) extends Rule
    // ⊥-introduction, or ⊥I: To prove ⊥, you must prove 𝝓 and ¬𝝓 (for any 𝝓 you like).
    // 1 𝝓 got this somehow
    // 2 ...
    // 3 ¬𝝓 and this
    // 4 ⊥ ⊥I(1, 3)
    case class FalsityIntro(orig: Int, negated: Int) extends Rule
    // ⊤-introduction, You can introduce ⊤ anywhere (for all the good it does you).
    case object TruthIntro extends Rule with ParserBridge0[Rule]
    // ↔-introduction, or ↔I: To prove 𝝓 ↔ φ, prove both 𝝓 → φ and φ → 𝝓.
    case class EquivIntro(
        leftImp: Int,
        rightImp: Int
    ) extends Rule
    // 𝝓(x) -> ∃a 𝝓(a)
    case class ExistsIntro(orig: Int) extends Rule
    // To introduce the sentence ∀x 𝝓 for some 𝝓(x), you introduce a new
    // constant, say c, not used in the proof so far, and prove 𝝓[c/x].
    case class ForallIntro(const: Int, concl: Int) extends Rule

    // ∧-elimination, ∧E: if you have (𝝓 ∧ φ), you can write down 𝝓 and/or φ
    case class AndElim(orig: Int) extends Rule
    // →-elimination, →E: you have 𝝓 and (𝝓 → φ), you can then write φ
    case class ImpliesElim(ass: Int, imp: Int) extends Rule
    // ∨-elimination, ∨E: prove by assuming 𝝓, then assume φ and get the same result
    case class OrElim(
        or: Int,
        leftAss: Int,
        leftConcl: Int,
        rightAss: Int,
        rightConcl: Int
    ) extends Rule
    // ¬-elimination, ¬E: 𝝓 and ¬𝝓 gives ⊥
    case class NotElim(negated: Int, orig: Int) extends Rule
    // ¬¬Elimination, ¬¬E: From ¬¬𝝓, deduce 𝝓
    // 1 ¬¬𝝓    proved this somehow
    // 2 𝝓      ¬¬E(1)
    case class DoubleNegElim(orig: Int) extends Rule
    // ⊥-elimination, ⊥E: This encode the fact that a contradiction can prove anything.
    // 1 ⊥ we got this
    // 2 𝝓 ⊥E(1)
    case class FalsityElim(bottom: Int) extends Rule
    // ↔-elimination, ↔E: From 𝝓 ↔ φ and 𝝓, you can prove φ. From 𝝓 ↔ φ and φ, you can prove 𝝓.
    case class EquivElim(equiv: Int, either: Int) extends Rule
    // ∃-elimination, or ∃E: Let 𝝓 be a formula. If you have managed to write down ∃x 𝝓,
    // you can prove a sentence φ from it by
    // • assuming 𝝓[c/x], where c is a new constant not used in the proof so far,
    // • proving φ from this assumption.
    case class ExistsElim(exists: Int, ass: Int, concl: Int) extends Rule
    // ∀-elimination, or ∀E: Let 𝝓(x) be a formula. If you have managed to
    // write down ∀x 𝝓, you can go on to write down ∀[t/x] for any closed
    // term t. (It's your choice which t!)
    case class ForallElim(orig: Int) extends Rule
    // ∀->Elimination: ∀x(f(x) -> g(x)) and f[t/x], use this rule to give you g[t/x] right away
    case class ForallImpElim(ass: Int, imp: Int) extends Rule

    // Law of excluded middle (p ∨ ¬p)
    case object LEM extends Rule with ParserBridge0[Rule]

    // Modus Tollens: From 𝝓 → φ and ¬φ, derive ¬𝝓.
    case class MT(imp: Int, not: Int) extends Rule
    // Proof by contradiction
    case class PC(orig: Int, bottom: Int) extends Rule
    // forall a. a = a
    case object Refl extends Rule with ParserBridge0[Rule]
    // a = b ^ expr(a) -> expr(b)
    case class EqSub(orig: Int, eq: Int) extends Rule
    // a = b <-> b = a
    case class Sym(orig: Int) extends Rule
    // Forall I const
    case object ForallIConst extends Rule with ParserBridge0[Rule]
    // given proposition
    case object Given extends Rule with ParserBridge0[Rule]
    // premise
    case object Premise extends Rule with ParserBridge0[Rule]
    // assumption
    case object Ass extends Rule with ParserBridge0[Rule]
    // the "tick"
    case class Tick(orig: Int) extends Rule
}
