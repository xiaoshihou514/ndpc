package ndpc.expr

import ndpc.expr.Formula._

object Rule {
    type ValidItem = Int | LF_

    // We can put either in this (avoid code dup!)
    // Still provides relatively strong type!
    sealed trait Rule[A <: ValidItem]

    // ∧-introduction, ∧I: you have to have already introduced both sides
    case class AndIntro[A](left: A, right: A) extends Rule[A]
    // →-introduction, →I: you assume 𝝓 and prove φ
    case class ImpliesIntro[A](ass: A, res: A) extends Rule[A]
    // ∨-introduction, ∨I: prove either side
    case class OrIntro[A](either: A) extends Rule[A]
    // ¬-introduction, ¬I: assume 𝝓 and get ⊥
    case class NotIntro[A](orig: A, bottom: A) extends Rule[A]
    // ¬¬introduction, ¬¬I: From 𝝓, deduce ¬¬𝝓
    // 1 𝝓      proved this somehow
    // 2 ¬¬𝝓    ¬¬I(1)
    case class DoubleNegIntro[A](orig: A) extends Rule[A]
    // ⊥-introduction, or ⊥I: To prove ⊥, you must prove 𝝓 and ¬𝝓 (for any 𝝓 you like).
    // 1 𝝓 got this somehow
    // 2 ...
    // 3 ¬𝝓 and this
    // 4 ⊥ ⊥I(1, 3)
    case class FalsityIntro[A](orig: A, negated: A) extends Rule[A]
    // ⊤-introduction, You can introduce ⊤ anywhere (for all the good it does you).
    case class TruthIntro() extends Rule
    // ↔-introduction, or ↔I: To prove 𝝓 ↔ φ, prove both 𝝓 → φ and φ → 𝝓.
    case class EquivIntro[A](
        leftImp: A,
        rightImp: A
    ) extends Rule[A]
    // 𝝓(x) -> ∃a 𝝓(a)
    case class ExistsIntro[A](orig: A) extends Rule[A]
    // To introduce the sentence ∀x 𝝓 for some 𝝓(x), you introduce a new
    // constant, say c, not used in the proof so far, and prove 𝝓[c/x].
    case class ForallIntro[A](orig: A, concl: A) extends Rule[A]

    // ∧-elimination, ∧E: if you have (𝝓 ∧ φ), you can write down 𝝓 and/or φ
    case class AndElim[A](orig: A) extends Rule[A]
    // →-elimination, →E: you have 𝝓 and (𝝓 → φ), you can then write φ
    case class ImpliesElim[A](ass: A, imp: A) extends Rule[A]
    // ∨-elimination, ∨E: prove by assuming 𝝓, then assume φ and get the same result
    case class OrElim[A](ifleft: A, ifright: A) extends Rule[A]
    // ¬-elimination, ¬E: 𝝓 and ¬𝝓 gives ⊥
    case class NotElim[A](orig: A, negated: A) extends Rule[A]
    // ¬¬Elimination, ¬¬E: From ¬¬𝝓, deduce 𝝓
    // 1 ¬¬𝝓    proved this somehow
    // 2 𝝓      ¬¬E(1)
    case class DoubleNegElim[A](orig: A) extends Rule[A]
    // ⊥-elimination, ⊥E: This encode the fact that a contradiction can prove anything.
    // 1 ⊥ we got this
    // 2 𝝓 ⊥E(1)
    case class FalsityElim[A](orig: A, neagated: A) extends Rule[A]
    // ↔-elimination, ↔E: From 𝝓 ↔ φ and 𝝓, you can prove φ. From 𝝓 ↔ φ and φ, you can prove 𝝓.
    case class EquivElim[A](leftImp: A, rightImp: A) extends Rule[A]
    // ∃-elimination, or ∃E: Let 𝝓 be a formula. If you have managed to write down ∃x 𝝓,
    // you can prove a sentence φ from it by
    // • assuming 𝝓[c/x], where c is a new constant not used in the proof so far,
    // • proving φ from this assumption.
    case class ExistsElim[A](ass: A, concl: A) extends Rule[A]
    // ∀-elimination, or ∀E: Let 𝝓(x) be a formula. If you have managed to
    // write down ∀x 𝝓, you can go on to write down ∀[t/x] for any closed
    // term t. (It’s your choice which t!)
    case class ForallElim[A](orig: A) extends Rule[A]
    // ∀->Elimination: ∀x(f(x) -> g(x)) and f[t/x], use this rule to give you g[t/x] right away
    case class ForallImpElim[A](ass: A, imp: A) extends Rule[A]

    // Law of excluded middle (p ∨ ¬p)
    case class LEM() extends Rule
    // Modus Tollens: From 𝝓 → φ and ¬φ, derive ¬𝝓.
    case class MT[A](imp: A, not: A) extends Rule[A]
    // Proof by contradiction
    case class PC[A](orig: A, neg: A) extends Rule[A]
    // forall a. a = a
    case class Refl() extends Rule
    // a = b ^ expr(a) -> expr(b)
    case class EqSub[A](orig: A, eq: A) extends Rule[A]
    // a = b <-> b = a
    case class Sym[A](orig: A) extends Rule[A]
    // Forall I const
    case class ForallIConst() extends Rule
    // given proposition
    case class Given() extends Rule
    // premise
    case class Premise() extends Rule
    // assumption
    case class Ass() extends Rule
    // the "tick"
    case class Tick[A](orig: A) extends Rule[A]
}
