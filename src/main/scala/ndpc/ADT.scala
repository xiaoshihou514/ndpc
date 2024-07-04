package ndpc

object ADT {
    case class Function(name: String, arity: Int)
    case class Predicate(name: String, arity: Int)
    // Definition 4.2 (term)
    // Fix a signature L.
    // 1. Any constant in L is an L-term.
    // 2. Any variable is an L-term.
    // 3. If f is an n-ary function symbol in L, and t1 , . . . , tn are L-terms, then
    // f (t1 , . . . , tn ) is an L-term.
    // 4. Nothing else is an L-term.
    enum LTerm:
        case Constant(name: String)
        case Variable(name: String)
        case FuncAp(f: Function, xs: List[LTerm])

    // Definition 4.3 (formula)
    // 1. If R is an n-ary predicate symbol in L, and t1 , . . . , tn are
    // L-terms, then R(t1 , . . . , tn ) is an atomic L-formula.
    // 2. If t, t' are L-terms then t = t' is an atomic L-formula.
    // 3. ⊤ and ⊥ are atomic L-formulas.
    // 4. If 𝝓, φ are L-formulas then so are (𝝓 ∧ φ), (𝝓 ∨ φ), (𝝓 → φ), and (𝝓 ↔ φ).
    // 5. If 𝝓 is an L-formula and x a variable, then (∀x 𝝓) and (∃x 𝝓) are L-formulas.
    // 6. Nothing else is an L-formula.
    enum LFormula:
        case PredAp(p: Predicate, xs: List[LTerm])
        case Eq(left: LTerm, right: LTerm)
        case Truth
        case Falsity
        case Not(pf: LFormula)
        case And(left: LFormula, right: LFormula)
        case Or(left: LFormula, right: LFormula)
        case Implies(left: LFormula, right: LFormula)
        case Equiv(left: LFormula, right: LFormula)
        case Forall(vars: String, body: LFormula)
        case Exists(vars: String, body: LFormula)
}
