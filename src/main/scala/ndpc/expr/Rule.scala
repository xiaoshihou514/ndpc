package ndpc.expr

import ndpc.expr.Formula._

object Rule {
    enum Rule:
        case Intro(i: Introduction)
        case Elim(e: Elimination)
        case Derived(s: Special)

    enum Introduction:
        // ∧-introduction, ∧I: you have to have alrready introduced both sides
        case And(left: BigInt, right: BigInt)
        // →-introduction, →I: you assume 𝝓 and prove φ
        case Implies(ass: BigInt, res: BigInt)
        // ∨-introduction, ∨I: prove either side
        case Or(either: BigInt)
        // ¬-introduction, ¬I: assume 𝝓 and get ⊥
        case Not(orig: BigInt, bottom: BigInt)
        // ¬¬introduction, ¬¬I: From 𝝓, deduce ¬¬𝝓
        // 1 𝝓      proved this somehow
        // 2 ¬¬𝝓    ¬¬I(1)
        case DoubleNeg(orig: BigInt)
        // ⊥-introduction, or ⊥I: To prove ⊥, you must prove 𝝓 and ¬𝝓 (for any 𝝓 you like).
        // 1 𝝓 got this somehow
        // 2 ...
        // 3 ¬𝝓 and this
        // 4 ⊥ ⊥I(1, 3)
        case Falsity(orig: BigInt, negated: BigInt)
        // ⊤-introduction, You can introduce ⊤ anywhere (for all the good it does you).
        case Truth
        // ↔-introduction, or ↔I: To prove 𝝓 ↔ φ, prove both 𝝓 → φ and φ → 𝝓.
        case Equiv(
            leftImp: BigInt,
            rightImp: BigInt
        )
        // 𝝓(x) -> ∃a 𝝓(a)
        case Exists(orig: BigInt)
        // To introduce the sentence ∀x 𝝓 for some 𝝓(x), you introduce a new
        // constant, say c, not used in the proof so far, and prove 𝝓[c/x].
        case Forall(orig: BigInt, concl: BigInt)

    enum Elimination:
        // ∧-elimination, ∧E: if you have (𝝓 ∧ φ), you can write down 𝝓 and/or φ
        case And(orig: BigInt)
        // →-elimination, →E: you have 𝝓 and (𝝓 → φ), you can then write φ
        case Implies(ass: BigInt, imp: BigInt)
        // ∨-elimination, ∨E: prove by assuming 𝝓, then assume φ and get the same result
        case Or(ifleft: BigInt, ifright: BigInt)
        // ¬-elimination, ¬E: 𝝓 and ¬𝝓 gives ⊥
        case Not(orig: BigInt, negated: BigInt)
        // ¬¬Elimination, ¬¬E: From ¬¬𝝓, deduce 𝝓
        // 1 ¬¬𝝓    proved this somehow
        // 2 𝝓      ¬¬E(1)
        case DoubleNeg(orig: BigInt)
        // ⊥-elimination, ⊥E: This encode the fact that a contradiction can prove anything.
        // 1 ⊥ we got this
        // 2 𝝓 ⊥E(1)
        case Falsity(orig: BigInt, neagated: BigInt)
        // ↔-elimination, ↔E: From 𝝓 ↔ φ and 𝝓, you can prove φ. From 𝝓 ↔ φ and φ, you can prove 𝝓.
        case Equiv(leftImp: BigInt, rightImp: BigInt)
        // ∃-elimination, or ∃E: Let 𝝓 be a formula. If you have managed to write down ∃x 𝝓,
        // you can prove a sentence φ from it by
        // • assuming 𝝓[c/x], where c is a new constant not used in the proof so far,
        // • proving φ from this assumption.
        case Exists(ass: BigInt, concl: BigInt)
        // ∀-elimination, or ∀E: Let 𝝓(x) be a formula. If you have managed to
        // write down ∀x 𝝓, you can go on to write down ∀[t/x] for any closed
        // term t. (It’s your choice which t!)
        case Forall(orig: BigInt)

    enum Special:
        // Law of excluded middle (p ∨ ¬p)
        case LEM
        // Modus Tollens: From 𝝓 → φ and ¬φ, derive ¬𝝓.
        case MT(imp: BigInt, not: BigInt)
        // Proof by contradiction
        case PC(orig: BigInt, neg: BigInt)
        // forall a. a = a
        case Refl
        // a = b ^ expr(a) -> expr(b)
        case EqSub(orig: BigInt, eq: BigInt)
        // a = b <-> b = a
        case Sym(orig: BigInt)
        // given proposition
        case Given
        // assumption
        case Ass
        // the "tick"
        case Tick(orig: BigInt)
}
