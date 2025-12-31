-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {p q r : Prop}
  (h1 : (p) → ((q) → (r)))
: ((p) → (q)) → ((p) → (r)) := by
  have h8 : ((p) → (q)) → ((p) → (r)) := by
    intro h2
    have h7 : (p) → (r) := by
      intro h3
      have h4 : q := h2 h3
      have h5 : (q) → (r) := h1 h3
      have h6 : r := h5 h4
      exact h6
    exact h7
  exact h8

end
