-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (f : Prop)
variable (B : Prop)
variable (W : Prop)
variable (P : Prop)


example 
  (h1 : (f) → ((B) ∨ (W)))
  (h2 : ¬ ((B) ∨ (P)))
  (h3 : (W) → (P))
: ¬ (f) := by
  have h14 : ¬ ((B) ∨ (W)) := by
    intro h4
    have h13 : False := by
      cases h4 with
      | inl h5 =>
           have h6 : (B) ∨ (P) := Or.inl h5
           have h7 : False := h2 h6
           have h8 : False := h7
           exact h8
      | inr h9 =>
           have h10 : P := h3 h9
           have h11 : (B) ∨ (P) := Or.inr h10
           have h12 : False := h2 h11
           exact h12
    exact h13
  have h15 : ¬ (f) := by
    intro tmp
    have htmp15 : (B) ∨ (W) := h1 tmp
    exact h14 htmp15
  exact h15

end
