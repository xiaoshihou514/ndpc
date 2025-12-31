-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {p q : Prop}
  (h1 : (p) ∧ (q))
: p := by
  have h2 : p := And.left h1
  exact h2

end
