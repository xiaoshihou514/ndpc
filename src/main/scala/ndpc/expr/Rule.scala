package ndpc.expr

import ndpc.expr.Formula._

object Rule {
    type ValidItem = BigInt | LF_

    // We can put either in this (avoid code dup!)
    // Still provides relatively strong type!
    enum Rule[A <: ValidItem]:
        case Intro[A <: ValidItem](i: Introduction[A]) extends Rule[A]
        case Elim[A <: ValidItem](e: Elimination[A]) extends Rule[A]
        case Builtin[A <: ValidItem](s: Special[A]) extends Rule[A]
        def fmap[B](f: A => B) = ???

    enum Introduction[A]:
        // ∧-introduction, ∧I: you have to have alrready introduced both sides
        case And(left: A, right: A)
        // →-introduction, →I: you assume 𝝓 and prove φ
        case Implies(ass: A, res: A)
        // ∨-introduction, ∨I: prove either side
        case Or(either: A)
        // ¬-introduction, ¬I: assume 𝝓 and get ⊥
        case Not(orig: A, bottom: A)
        // ¬¬introduction, ¬¬I: From 𝝓, deduce ¬¬𝝓
        // 1 𝝓      proved this somehow
        // 2 ¬¬𝝓    ¬¬I(1)
        case DoubleNeg(orig: A)
        // ⊥-introduction, or ⊥I: To prove ⊥, you must prove 𝝓 and ¬𝝓 (for any 𝝓 you like).
        // 1 𝝓 got this somehow
        // 2 ...
        // 3 ¬𝝓 and this
        // 4 ⊥ ⊥I(1, 3)
        case Falsity(orig: A, negated: A)
        // ⊤-introduction, You can introduce ⊤ anywhere (for all the good it does you).
        case Truth()
        // ↔-introduction, or ↔I: To prove 𝝓 ↔ φ, prove both 𝝓 → φ and φ → 𝝓.
        case Equiv(
            leftImp: A,
            rightImp: A
        )
        // 𝝓(x) -> ∃a 𝝓(a)
        case Exists(orig: A)
        // To introduce the sentence ∀x 𝝓 for some 𝝓(x), you introduce a new
        // constant, say c, not used in the proof so far, and prove 𝝓[c/x].
        case Forall(orig: A, concl: A)

    enum Elimination[A]:
        // ∧-elimination, ∧E: if you have (𝝓 ∧ φ), you can write down 𝝓 and/or φ
        case And(orig: A)
        // →-elimination, →E: you have 𝝓 and (𝝓 → φ), you can then write φ
        case Implies(ass: A, imp: A)
        // ∨-elimination, ∨E: prove by assuming 𝝓, then assume φ and get the same result
        case Or(ifleft: A, ifright: A)
        // ¬-elimination, ¬E: 𝝓 and ¬𝝓 gives ⊥
        case Not(orig: A, negated: A)
        // ¬¬Elimination, ¬¬E: From ¬¬𝝓, deduce 𝝓
        // 1 ¬¬𝝓    proved this somehow
        // 2 𝝓      ¬¬E(1)
        case DoubleNeg(orig: A)
        // ⊥-elimination, ⊥E: This encode the fact that a contradiction can prove anything.
        // 1 ⊥ we got this
        // 2 𝝓 ⊥E(1)
        case Falsity(orig: A, neagated: A)
        // ↔-elimination, ↔E: From 𝝓 ↔ φ and 𝝓, you can prove φ. From 𝝓 ↔ φ and φ, you can prove 𝝓.
        case Equiv(leftImp: A, rightImp: A)
        // ∃-elimination, or ∃E: Let 𝝓 be a formula. If you have managed to write down ∃x 𝝓,
        // you can prove a sentence φ from it by
        // • assuming 𝝓[c/x], where c is a new constant not used in the proof so far,
        // • proving φ from this assumption.
        case Exists(ass: A, concl: A)
        // ∀-elimination, or ∀E: Let 𝝓(x) be a formula. If you have managed to
        // write down ∀x 𝝓, you can go on to write down ∀[t/x] for any closed
        // term t. (It’s your choice which t!)
        case Forall(orig: A)
        // ∀->Elimination: ∀x(f(x) -> g(x)) and f[t/x], use this rule to give you g[t/x] right away
        case ForallImp(ass: A, imp: A)

    enum Special[A]:
        // Law of excluded middle (p ∨ ¬p)
        case LEM()
        // Modus Tollens: From 𝝓 → φ and ¬φ, derive ¬𝝓.
        case MT(imp: A, not: A)
        // Proof by contradiction
        case PC(orig: A, neg: A)
        // forall a. a = a
        case Refl()
        // a = b ^ expr(a) -> expr(b)
        case EqSub(orig: A, eq: A)
        // a = b <-> b = a
        case Sym(orig: A)
        // Forall I const
        case ForallIConst()
        // given proposition
        case Given()
        // premise
        case Premise()
        // assumption
        case Ass()
        // the "tick"
        case Tick(orig: A)
}
