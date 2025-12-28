-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {t P R Q : Prop}
  (h1 : ¬ (t))
  (h2 : (P) → (¬ ((R) ∨ (Q))))
  (h3 : (P) → ((R) ∨ (t)))
: (P) → (¬ (Q)) := by
  have h16 : (P) → (¬ (Q)) := by
    have h5 : (R) ∨ (t) := h4 h3
    have h6 : ¬ ((R) ∨ (Q)) := h4 h2
    cases h5 with
 inl h7 =>
         have h13 : False := h1 h12
         have h14 : ¬ (Q) := False.elim 13
         exact h11
 inr h12 =>
    
         exact h14
    exact h13
  exact h16

end
