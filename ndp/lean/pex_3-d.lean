-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {f B W P : Prop}
  (h1 : (f) → ((B) ∨ (W)))
  (h2 : ¬ ((B) ∨ (P)))
  (h3 : (W) → (P))
: ¬ (f) := by
  have h14 : ¬ ((B) ∨ (W)) := by
    intro h4
    cases h4 with
 inl h5 =>
         have h10 : P := h9 h3
         have h11 : (B) ∨ (P) := Or.inr 10
         have h12 : False := h2 h11
         exact h8
 inr h9 =>
    
         exact h12
    exact h13
  have h15 : ¬ (f) := by
    intro hA
    have hB : (B) ∨ (W) := h1 hA
    exact h14 hB
  exact h15

end
