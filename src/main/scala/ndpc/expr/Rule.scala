package ndpc.expr

import ndpc.expr.Formula._

object Rule {
    type ValidItem = Int | LF_

    // We can put either in this (avoid code dup!)
    // Still provides relatively strong type!
    sealed trait Rule[A <: ValidItem]

    // ∧-introduction, ∧I: you have to have already introduced both sides
    case class AndIntro[A <: ValidItem](left: A, right: A) extends Rule[A] {
        override def toString(): String = s"^I($left, $right)"
    }
    // →-introduction, →I: you assume 𝝓 and prove φ
    case class ImpliesIntro[A <: ValidItem](ass: A, res: A) extends Rule[A] {
        override def toString(): String = s"->I($ass, $res)"
    }
    // ∨-introduction, ∨I: prove either side
    case class OrIntro[A <: ValidItem](either: A) extends Rule[A] {
        override def toString(): String = s"/I($either)"
    }
    // ¬-introduction, ¬I: assume 𝝓 and get ⊥
    case class NotIntro[A <: ValidItem](orig: A, bottom: A) extends Rule[A] {
        override def toString(): String = s"~I($orig, $bottom)"
    }
    // ¬¬introduction, ¬¬I: From 𝝓, deduce ¬¬𝝓
    // 1 𝝓      proved this somehow
    // 2 ¬¬𝝓    ¬¬I(1)
    case class DoubleNegIntro[A <: ValidItem](orig: A) extends Rule[A] {
        override def toString(): String = s"~~I($orig)"
    }
    // ⊥-introduction, or ⊥I: To prove ⊥, you must prove 𝝓 and ¬𝝓 (for any 𝝓 you like).
    // 1 𝝓 got this somehow
    // 2 ...
    // 3 ¬𝝓 and this
    // 4 ⊥ ⊥I(1, 3)
    case class FalsityIntro[A <: ValidItem](orig: A, negated: A)
        extends Rule[A] {
        override def toString(): String = s"FI($orig, $negated)"
    }
    // ⊤-introduction, You can introduce ⊤ anywhere (for all the good it does you).
    case class TruthIntro[A <: ValidItem]() extends Rule[A] {
        override def toString(): String = s"TI"
    }
    // ↔-introduction, or ↔I: To prove 𝝓 ↔ φ, prove both 𝝓 → φ and φ → 𝝓.
    case class EquivIntro[A <: ValidItem](
        leftImp: A,
        rightImp: A
    ) extends Rule[A] {
        override def toString(): String = s"<->I($leftImp, $rightImp)"
    }
    // 𝝓(x) -> ∃a 𝝓(a)
    case class ExistsIntro[A <: ValidItem](orig: A) extends Rule[A] {
        override def toString(): String = s"existsI($orig)"
    }
    // To introduce the sentence ∀x 𝝓 for some 𝝓(x), you introduce a new
    // constant, say c, not used in the proof so far, and prove 𝝓[c/x].
    case class ForallIntro[A <: ValidItem](const: A, concl: A) extends Rule[A] {
        override def toString(): String = s"forallI($const, $concl)"
    }

    // ∧-elimination, ∧E: if you have (𝝓 ∧ φ), you can write down 𝝓 and/or φ
    case class AndElim[A <: ValidItem](orig: A) extends Rule[A] {
        override def toString(): String = s"^E($orig)"
    }
    // →-elimination, →E: you have 𝝓 and (𝝓 → φ), you can then write φ
    case class ImpliesElim[A <: ValidItem](ass: A, imp: A) extends Rule[A] {
        override def toString(): String = s"->E($ass, $imp)"
    }
    // ∨-elimination, ∨E: prove by assuming 𝝓, then assume φ and get the same result
    case class OrElim[A <: ValidItem](
        or: A,
        leftAss: A,
        leftConcl: A,
        rightAss: A,
        rightConcl: A
    ) extends Rule[A] {
        override def toString(): String =
            s"/E($or, $leftAss, $leftConcl, $rightAss, $rightConcl)"
    }
    // ¬-elimination, ¬E: 𝝓 and ¬𝝓 gives ⊥
    case class NotElim[A <: ValidItem](orig: A, negated: A) extends Rule[A] {
        override def toString(): String = s"~E($orig, $negated)"
    }
    // ¬¬Elimination, ¬¬E: From ¬¬𝝓, deduce 𝝓
    // 1 ¬¬𝝓    proved this somehow
    // 2 𝝓      ¬¬E(1)
    case class DoubleNegElim[A <: ValidItem](orig: A) extends Rule[A] {
        override def toString(): String = s"~~E($orig)"
    }
    // ⊥-elimination, ⊥E: This encode the fact that a contradiction can prove anything.
    // 1 ⊥ we got this
    // 2 𝝓 ⊥E(1)
    case class FalsityElim[A <: ValidItem](bottom: A) extends Rule[A] {
        override def toString(): String = s"FE($bottom)"
    }
    // ↔-elimination, ↔E: From 𝝓 ↔ φ and 𝝓, you can prove φ. From 𝝓 ↔ φ and φ, you can prove 𝝓.
    case class EquivElim[A <: ValidItem](equiv: A, either: A) extends Rule[A] {
        override def toString(): String = s"<->E($equiv, $either)"
    }
    // ∃-elimination, or ∃E: Let 𝝓 be a formula. If you have managed to write down ∃x 𝝓,
    // you can prove a sentence φ from it by
    // • assuming 𝝓[c/x], where c is a new constant not used in the proof so far,
    // • proving φ from this assumption.
    case class ExistsElim[A <: ValidItem](ass: A, concl: A) extends Rule[A] {
        override def toString(): String = s"existsE($ass, $concl)"
    }
    // ∀-elimination, or ∀E: Let 𝝓(x) be a formula. If you have managed to
    // write down ∀x 𝝓, you can go on to write down ∀[t/x] for any closed
    // term t. (It’s your choice which t!)
    case class ForallElim[A <: ValidItem](orig: A) extends Rule[A] {
        override def toString(): String = s"forallE($orig)"
    }
    // ∀->Elimination: ∀x(f(x) -> g(x)) and f[t/x], use this rule to give you g[t/x] right away
    case class ForallImpElim[A <: ValidItem](ass: A, imp: A) extends Rule[A] {
        override def toString(): String = s"forall->E($ass, $imp)"
    }

    // Law of excluded middle (p ∨ ¬p)
    case class LEM[A <: ValidItem]() extends Rule[A] {
        override def toString(): String = "LEM"
    }
    // Modus Tollens: From 𝝓 → φ and ¬φ, derive ¬𝝓.
    case class MT[A <: ValidItem](imp: A, not: A) extends Rule[A] {
        override def toString(): String = "MT"
    }
    // Proof by contradiction
    case class PC[A <: ValidItem](orig: A, neg: A) extends Rule[A] {
        override def toString(): String = s"PC($orig, $neg)"
    }
    // forall a. a = a
    case class Refl[A <: ValidItem]() extends Rule[A] {
        override def toString(): String = s"Refl"
    }
    // a = b ^ expr(a) -> expr(b)
    case class EqSub[A <: ValidItem](orig: A, eq: A) extends Rule[A] {
        override def toString(): String = s"=sub($orig, $eq)"
    }
    // a = b <-> b = a
    case class Sym[A <: ValidItem](orig: A) extends Rule[A] {
        override def toString(): String = s"sym($orig)"
    }
    // Forall I const
    case class ForallIConst[A <: ValidItem]() extends Rule[A] {
        override def toString(): String = "forall I const"
    }
    // given proposition
    case class Given[A <: ValidItem]() extends Rule[A] {
        override def toString(): String = "given"
    }
    // premise
    case class Premise[A <: ValidItem]() extends Rule[A] {
        override def toString(): String = "premise"
    }
    // assumption
    case class Ass[A <: ValidItem]() extends Rule[A] {
        override def toString(): String = "ass"
    }
    // the "tick"
    case class Tick[A <: ValidItem](orig: A) extends Rule[A] {
        override def toString(): String = s"tick($orig)"
    }
}
