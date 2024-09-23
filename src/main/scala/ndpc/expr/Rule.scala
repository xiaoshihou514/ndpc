package ndpc.expr

import ndpc.expr.Formula._

object Rule {
    sealed trait Rule

    // ∧-introduction, ∧I: you have to have already introduced both sides
    case class AndIntro(left: Int, right: Int) extends Rule {
        override def toString(): String = s"^I($left, $right)"
    }
    // →-introduction, →I: you assume 𝝓 and prove φ
    case class ImpliesIntro(ass: Int, res: Int) extends Rule {
        override def toString(): String = s"->I($ass, $res)"
    }
    // ∨-introduction, ∨I: prove either side
    case class OrIntro(either: Int) extends Rule {
        override def toString(): String = s"/I($either)"
    }
    // ¬-introduction, ¬I: assume 𝝓 and get ⊥
    case class NotIntro(orig: Int, bottom: Int) extends Rule {
        override def toString(): String = s"~I($orig, $bottom)"
    }
    // ¬¬introduction, ¬¬I: From 𝝓, deduce ¬¬𝝓
    // 1 𝝓      proved this somehow
    // 2 ¬¬𝝓    ¬¬I(1)
    case class DoubleNegIntro(orig: Int) extends Rule {
        override def toString(): String = s"~~I($orig)"
    }
    // ⊥-introduction, or ⊥I: To prove ⊥, you must prove 𝝓 and ¬𝝓 (for any 𝝓 you like).
    // 1 𝝓 got this somehow
    // 2 ...
    // 3 ¬𝝓 and this
    // 4 ⊥ ⊥I(1, 3)
    case class FalsityIntro(orig: Int, negated: Int) extends Rule {
        override def toString(): String = s"FI($orig, $negated)"
    }
    // ⊤-introduction, You can introduce ⊤ anywhere (for all the good it does you).
    case class TruthIntro() extends Rule {
        override def toString(): String = s"TI"
    }
    // ↔-introduction, or ↔I: To prove 𝝓 ↔ φ, prove both 𝝓 → φ and φ → 𝝓.
    case class EquivIntro(
        leftImp: Int,
        rightImp: Int
    ) extends Rule {
        override def toString(): String = s"<->I($leftImp, $rightImp)"
    }
    // 𝝓(x) -> ∃a 𝝓(a)
    case class ExistsIntro(orig: Int) extends Rule {
        override def toString(): String = s"existsI($orig)"
    }
    // To introduce the sentence ∀x 𝝓 for some 𝝓(x), you introduce a new
    // constant, say c, not used in the proof so far, and prove 𝝓[c/x].
    case class ForallIntro(const: Int, concl: Int) extends Rule {
        override def toString(): String = s"forallI($const, $concl)"
    }

    // ∧-elimination, ∧E: if you have (𝝓 ∧ φ), you can write down 𝝓 and/or φ
    case class AndElim(orig: Int) extends Rule {
        override def toString(): String = s"^E($orig)"
    }
    // →-elimination, →E: you have 𝝓 and (𝝓 → φ), you can then write φ
    case class ImpliesElim(ass: Int, imp: Int) extends Rule {
        override def toString(): String = s"->E($ass, $imp)"
    }
    // ∨-elimination, ∨E: prove by assuming 𝝓, then assume φ and get the same result
    case class OrElim(
        or: Int,
        leftAss: Int,
        leftConcl: Int,
        rightAss: Int,
        rightConcl: Int
    ) extends Rule {
        override def toString(): String =
            s"/E($or, $leftAss, $leftConcl, $rightAss, $rightConcl)"
    }
    // ¬-elimination, ¬E: 𝝓 and ¬𝝓 gives ⊥
    case class NotElim(negated: Int, orig: Int) extends Rule {
        override def toString(): String = s"~E($negated, $orig)"
    }
    // ¬¬Elimination, ¬¬E: From ¬¬𝝓, deduce 𝝓
    // 1 ¬¬𝝓    proved this somehow
    // 2 𝝓      ¬¬E(1)
    case class DoubleNegElim(orig: Int) extends Rule {
        override def toString(): String = s"~~E($orig)"
    }
    // ⊥-elimination, ⊥E: This encode the fact that a contradiction can prove anything.
    // 1 ⊥ we got this
    // 2 𝝓 ⊥E(1)
    case class FalsityElim(bottom: Int) extends Rule {
        override def toString(): String = s"FE($bottom)"
    }
    // ↔-elimination, ↔E: From 𝝓 ↔ φ and 𝝓, you can prove φ. From 𝝓 ↔ φ and φ, you can prove 𝝓.
    case class EquivElim(equiv: Int, either: Int) extends Rule {
        override def toString(): String = s"<->E($equiv, $either)"
    }
    // ∃-elimination, or ∃E: Let 𝝓 be a formula. If you have managed to write down ∃x 𝝓,
    // you can prove a sentence φ from it by
    // • assuming 𝝓[c/x], where c is a new constant not used in the proof so far,
    // • proving φ from this assumption.
    case class ExistsElim(exists: Int, ass: Int, concl: Int) extends Rule {
        override def toString(): String = s"existsE($exists, $ass, $concl)"
    }
    // ∀-elimination, or ∀E: Let 𝝓(x) be a formula. If you have managed to
    // write down ∀x 𝝓, you can go on to write down ∀[t/x] for any closed
    // term t. (It’s your choice which t!)
    case class ForallElim(orig: Int) extends Rule {
        override def toString(): String = s"forallE($orig)"
    }
    // ∀->Elimination: ∀x(f(x) -> g(x)) and f[t/x], use this rule to give you g[t/x] right away
    case class ForallImpElim(ass: Int, imp: Int) extends Rule {
        override def toString(): String = s"forall->E($ass, $imp)"
    }

    // Law of excluded middle (p ∨ ¬p)
    case class LEM() extends Rule {
        override def toString(): String = "LEM"
    }
    // Modus Tollens: From 𝝓 → φ and ¬φ, derive ¬𝝓.
    case class MT(imp: Int, not: Int) extends Rule {
        override def toString(): String = "MT"
    }
    // Proof by contradiction
    case class PC(orig: Int, bottom: Int) extends Rule {
        override def toString(): String = s"PC($orig, $bottom)"
    }
    // forall a. a = a
    case class Refl() extends Rule {
        override def toString(): String = s"Refl"
    }
    // a = b ^ expr(a) -> expr(b)
    case class EqSub(orig: Int, eq: Int) extends Rule {
        override def toString(): String = s"=sub($orig, $eq)"
    }
    // a = b <-> b = a
    case class Sym(orig: Int) extends Rule {
        override def toString(): String = s"sym($orig)"
    }
    // Forall I const
    case class ForallIConst() extends Rule {
        override def toString(): String = "forall I const"
    }
    // given proposition
    case class Given() extends Rule {
        override def toString(): String = "given"
    }
    // premise
    case class Premise() extends Rule {
        override def toString(): String = "premise"
    }
    // assumption
    case class Ass() extends Rule {
        override def toString(): String = "ass"
    }
    // the "tick"
    case class Tick(orig: Int) extends Rule {
        override def toString(): String = s"tick($orig)"
    }
}
