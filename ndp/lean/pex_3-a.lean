-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (P : Prop)
variable (Q : Prop)
variable (R : Prop)
variable (S : Prop)


example 
  (h1 : (P) → (Q))
  (h2 : (¬ (P)) → (R))
  (h3 : (Q) → (S))
  (h4 : (R) → (S))
: S := by
  have h5 : (P) ∨ (¬ (P)) := em P
  have h13 : S := by
    cases h5 with
    | inl h6 =>
         have h7 : Q := h1 h6
         have h8 : S := h3 h7
         have h9 : S := h8
         exact h9
    | inr h10 =>
         have h11 : R := h2 h10
         have h12 : S := h4 h11
         exact h12
  exact h13

end
