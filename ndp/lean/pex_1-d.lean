-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {p q r : Prop}
  (h1 : (p) → ((q) → (r)))
: ((p) → (q)) → ((p) → (r)) := by
  have h8 : ((p) → (q)) → ((p) → (r)) := by
    have h7 : (p) → (r) := by
      have h4 : q := h3 h2
      have h5 : (q) → (r) := h3 h1
      have h6 : r := h4 h5
      exact h6
    exact h7
  exact h8

end
