-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {p q : Prop}

: (p) → ((q) → (p)) := by
  have h5 : (p) → ((q) → (p)) := by
    intro h1
    have h4 : (q) → (p) := by
      intro h2
      have h3 : p := h1
      exact h3
    exact h4
  exact h5

end
