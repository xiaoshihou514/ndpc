-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (R : Prop)
variable (I : Prop)
variable (f : Prop)


example 
  (h1 : (R) → (¬ (I)))
  (h2 : (I) ∨ (f))
  (h3 : ¬ (f))
: ¬ (R) := by
  have h11 : ¬ (R) := by
    cases h2 with
    | inl h4 =>
         have h5 : ¬ (¬ (I)) := by
           intro tmp5
           exact tmp5 h4
         have h6 : ¬ (R) := by
           intro tmp
           have htmp6 : ¬ (I) := h1 tmp
           exact h5 htmp6
         have h7 : ¬ (R) := h6
         exact h7
    | inr h8 =>
         have h9 : False := h3 h8
         have h10 : ¬ (R) := False.elim h9
         exact h10
  exact h11

end
