-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {p q : Prop}

: ((p) ∧ (q)) → (p) := by
  have h3 : ((p) ∧ (q)) → (p) := by
    have h2 : p := And.left 1
    exact h3
  exact h3

end
