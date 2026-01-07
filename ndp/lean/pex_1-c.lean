-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (p : Prop)
variable (q : Prop)


example 
  (h1 : p)
: (q) → ((p) ∧ (q)) := by
  have h4 : (q) → ((p) ∧ (q)) := by
    intro h2
    have h3 : (p) ∧ (q) := And.intro h1 h2
    exact h3
  exact h4

end
