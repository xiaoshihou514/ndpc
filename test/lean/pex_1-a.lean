-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (p : Prop)
variable (q : Prop)


example 
  (h1 : (p) ∧ (q))
: p := by
  have h2 : p := And.left h1
  exact h2

end
