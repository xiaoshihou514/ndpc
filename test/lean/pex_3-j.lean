-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (K : Prop)
variable (B : Prop)


example 
  (h1 : (K) ↔ (¬ (B)))
: ¬ ((K) ↔ (B)) := by
  have h20 : ¬ ((K) ↔ (B)) := by
    intro h2
    have h3 : (K) ∨ (¬ (K)) := em K
    have h19 : False := by
      cases h3 with
      | inl h4 =>
           have h5 : B := h2.mp h4
           have h6 : ¬ (B) := h1.mp h4
           have h7 : False := h6 h5
           have h8 : False := h7
           exact h8
      | inr h9 =>
           have h10 : (B) ∨ (¬ (B)) := em B
           have h18 : False := by
             cases h10 with
             | inl h11 =>
                  have h12 : K := h2.mpr h11
                  have h13 : False := h9 h12
                  have h14 : False := h13
                  exact h14
             | inr h15 =>
                  have h16 : K := h1.mpr h15
                  have h17 : False := h9 h16
                  exact h17
           exact h18
    exact h19
  exact h20

end
