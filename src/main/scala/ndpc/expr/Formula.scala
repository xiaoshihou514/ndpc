package ndpc.expr

object Formula {
    case class Function(name: String, arity: Int)
    case class Predicate(name: String, arity: Int)
    // Definition 4.2 (term)
    // Fix a signature L.
    sealed trait LTerm
    // 1. Any constant in L is an L-term.
    // 2. Any variable is an L-term.
    // here we just consider variable since
    //  1. it's hard to differentiate the semantics during parsing
    //  2. it's not too useful for use to differentiate the two
    case class Variable(name: String) extends LTerm {
        override def toString(): String = name
    }
    // 3. If f is an n-ary function symbol in L, and t1...tn are L-terms, then f (t1...tn) is an L-term.
    case class FuncAp(f: Function, xs: List[LTerm]) extends LTerm {
        override def toString(): String = s"${f.name}(${xs.mkString(", ")})"
    }
    // 4. Nothing else is an L-term.

    // Definition 4.3 (formula)
    sealed trait LFormula[T]
    // 1. If R is an n-ary predicate symbol in L, and t1...tn are
    // L-terms, then R(t1...tn) is an atomic L-formula.
    case class PredAp(p: Predicate, xs: List[LTerm]) extends LFormula[PredAp] {
        override def toString(): String = s"${p.name}(${xs.mkString(", ")})"
    }

    // 2. If t, t' are L-terms then t = t' is an atomic L-formula.
    case class Eq(left: LTerm, right: LTerm) extends LFormula[Eq] {
        override def toString(): String = s"$left = $right"
    }

    // 3. ⊤ and ⊥ are atomic L-formulas.
    case class Truth() extends LFormula[true] {
        override def toString(): String = "T"
    }
    case class Falsity() extends LFormula[false] {
        override def toString(): String = "F"
    }

    // 4. If 𝝓, φ are L-formulas then so are ¬𝝓, (𝝓 ∧ φ), (𝝓 ∨ φ), (𝝓 → φ), and (𝝓 ↔ φ).
    case class Not[A](pf: LFormula[A]) extends LFormula[Not[A]] {
        override def toString(): String = s"~($pf)"
    }

    case class And[A, B](left: LFormula[A], right: LFormula[B])
        extends LFormula[And[A, B]] {
        override def toString(): String = s"$left ^ $right"
    }

    case class Or[A, B](left: LFormula[A], right: LFormula[B])
        extends LFormula[Or[A, B]] {
        override def toString(): String = s"$left / $right"
    }

    case class Implies[A, B](left: LFormula[A], right: LFormula[B])
        extends LFormula[Implies[A, B]] {
        override def toString(): String = s"$left -> $right"
    }

    case class Equiv[A, B](left: LFormula[A], right: LFormula[B])
        extends LFormula[Equiv[A, B]] {
        override def toString(): String = s"$left <-> $right"
    }

    // 5. If 𝝓 is an L-formula and x a variable, then (∀x 𝝓) and (∃x 𝝓) are L-formulas.
    case class Forall[A](
        vars: List[String],
        body: LFormula[A]
    ) extends LFormula[Forall[A]] {
        override def toString(): String =
            s"forall ${vars.mkString(". ")} ($body)"
    }

    case class Exists[A](
        vars: List[String],
        body: LFormula[A]
    ) extends LFormula[Exists[A]] {
        override def toString(): String =
            s"exists ${vars.mkString(". ")} ($body)"
    }

    type LF[A] = LFormula[A]
    type LF_ = LFormula[_]
}
