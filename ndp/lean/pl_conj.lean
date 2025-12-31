-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {p q r : Prop}
  (h1 : (p) ∧ (q))
  (h2 : r)
: (q) ∧ (r) := by
  have h3 : q := And.right h1
  have h4 : (q) ∧ (r) := And.intro h3 h2
  exact h4

end
