-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {p q r : Prop}
  (h1 : ((p) ∧ (q)) → (r))
: (p) → ((q) → (r)) := by
  have h7 : (p) → ((q) → (r)) := by
    intro h2
    have h6 : (q) → (r) := by
      intro h3
      have h4 : (p) ∧ (q) := And.intro h2 h3
      have h5 : r := h1 h4
      exact h5
    exact h6
  exact h7

end
