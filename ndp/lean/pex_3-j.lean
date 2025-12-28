-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {K B : Prop}
  (h1 : (K) ↔ (¬ (B)))
: ¬ ((K) ↔ (B)) := by
  have h20 : ¬ ((K) ↔ (B)) := by
    intro h2
    have h3 : (K) ∨ (¬ (K)) := em K
    cases h3 with
 inl h4 =>
         have h10 : (B) ∨ (¬ (B)) := em B
         cases h10 with
     inl h11 =>
              have h16 : K := h1.mp 15
              have h17 : False := h9 h16
              exact h14
     inr h15 =>
         
              exact h17
         exact h8
 inr h9 =>
    
         exact h18
    exact h19
  exact h20

end
