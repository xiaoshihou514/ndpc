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
    case class Variable(name: String) extends LTerm
    // 3. If f is an n-ary function symbol in L, and t1...tn are L-terms, then f (t1...tn) is an L-term.
    case class FuncAp(f: Function, xs: List[LTerm]) extends LTerm
    // 4. Nothing else is an L-term.

    // Definition 4.3 (formula)
    sealed trait LFormula[T]
    // 1. If R is an n-ary predicate symbol in L, and t1...tn are
    // L-terms, then R(t1...tn) is an atomic L-formula.
    case class PredAp(p: Predicate, xs: List[LTerm]) extends LFormula[PredAp]

    // 2. If t, t' are L-terms then t = t' is an atomic L-formula.
    case class Eq(left: LTerm, right: LTerm) extends LFormula[Eq]

    // 3. ⊤ and ⊥ are atomic L-formulas.
    case class Truth() extends LFormula[true]
    case class Falsity() extends LFormula[false]

    // 4. If 𝝓, φ are L-formulas then so are ¬𝝓, (𝝓 ∧ φ), (𝝓 ∨ φ), (𝝓 → φ), and (𝝓 ↔ φ).
    case class Not[A](pf: LFormula[A]) extends LFormula[Not[A]]

    case class And[A, B](left: LFormula[A], right: LFormula[B])
        extends LFormula[And[A, B]]

    case class Or[A, B](left: LFormula[A], right: LFormula[B])
        extends LFormula[Or[A, B]]

    case class Implies[A, B](left: LFormula[A], right: LFormula[B])
        extends LFormula[Implies[A, B]]

    case class Equiv[A, B](left: LFormula[A], right: LFormula[B])
        extends LFormula[Equiv[A, B]]

    // 5. If 𝝓 is an L-formula and x a variable, then (∀x 𝝓) and (∃x 𝝓) are L-formulas.
    case class Forall[A](
        vars: List[String],
        body: LFormula[A]
    ) extends LFormula[Forall[A]]

    case class Exists[A](
        vars: List[String],
        body: LFormula[A]
    ) extends LFormula[Exists[A]]

    type LF[A] = LFormula[A]
    type LF_ = LFormula[_]
}
