-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {p q : Prop}
  (h1 : p)
: (q) → ((p) ∧ (q)) := by
  have h4 : (q) → ((p) ∧ (q)) := by
    have h3 : (p) ∧ (q) := And.Intro 1 2
    exact h3
  exact h4

end
