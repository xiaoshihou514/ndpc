package ndpc

object Formula {
    case class Function(name: String, arity: Int)
    case class Predicate(name: String, arity: Int)
    // Definition 4.2 (term)
    // Fix a signature L.
    enum LTerm:
        // 1. Any constant in L is an L-term.
        // 2. Any variable is an L-term.
        // here we just consider variable since
        //  1. it's hard to differentiate the semantics during parsing
        //  2. it's not too useful for use to differentiate the two
        case Variable(name: String)
        // 3. If f is an n-ary function symbol in L, and t1...tn are L-terms, then f (t1...tn) is an L-term.
        case FuncAp(f: Function, xs: List[LTerm])
        // 4. Nothing else is an L-term.

    // Definition 4.3 (formula)
    enum LFormula[T]:
        // 1. If R is an n-ary predicate symbol in L, and t1...tn are
        // L-terms, then R(t1...tn) is an atomic L-formula.
        case PredAp(p: Predicate, xs: List[LTerm]) extends LFormula[PredAp]

        // 2. If t, t' are L-terms then t = t' is an atomic L-formula.
        case Eq(left: LTerm, right: LTerm) extends LFormula[Eq]

        // 3. ⊤ and ⊥ are atomic L-formulas.
        case Truth extends LFormula[true]
        case Falsity extends LFormula[false]

        // 4. If 𝝓, φ are L-formulas then so are ¬𝝓, (𝝓 ∧ φ), (𝝓 ∨ φ), (𝝓 → φ), and (𝝓 ↔ φ).
        case Not[A](pf: LFormula[A]) extends LFormula[Not[A]]

        case And[A, B](left: LFormula[A], right: LFormula[B])
            extends LFormula[And[A, B]]

        case Or[A, B](left: LFormula[A], right: LFormula[B])
            extends LFormula[Or[A, B]]

        case Implies[A, B](left: LFormula[A], right: LFormula[B])
            extends LFormula[Implies[A, B]]

        case Equiv[A, B](left: LFormula[A], right: LFormula[B])
            extends LFormula[Equiv[A, B]]

        // 5. If 𝝓 is an L-formula and x a variable, then (∀x 𝝓) and (∃x 𝝓) are L-formulas.
        case Forall[A](
            vars: List[String],
            body: LFormula[A]
        ) extends LFormula[Forall[A]]

        case Exists[A](
            vars: List[String],
            body: LFormula[A]
        ) extends LFormula[Exists[A]]

        // 6. Nothing else is an L-formula.
}
