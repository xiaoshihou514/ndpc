-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (p : Prop)
variable (q : Prop)


example 
  (h1 : p)
: ((p) ∧ (q)) ∨ ((p) ∧ (¬ (q))) := by
  have h2 : (q) ∨ (¬ (q)) := em q
  have h10 : ((p) ∧ (q)) ∨ ((p) ∧ (¬ (q))) := by
    cases h2 with
    | inl h3 =>
         have h4 : (p) ∧ (q) := And.intro h1 h3
         have h5 : ((p) ∧ (q)) ∨ ((p) ∧ (¬ (q))) := Or.inl h4
         have h6 : ((p) ∧ (q)) ∨ ((p) ∧ (¬ (q))) := h5
         exact h6
    | inr h7 =>
         have h8 : (p) ∧ (¬ (q)) := And.intro h1 h7
         have h9 : ((p) ∧ (q)) ∨ ((p) ∧ (¬ (q))) := Or.inr h8
         exact h9
  exact h10

end
