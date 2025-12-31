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
    intro h4
    have h5 : (R) ∨ (t) := h3 h4
    have h6 : ¬ ((R) ∨ (Q)) := h2 h4
    have h15 : ¬ (Q) := by
      cases h5 with
      | inl h7 =>
           have h8 : (R) ∨ (Q) := Or.inl h7
           have h9 : False := h6 h8
           have h10 : ¬ (Q) := False.elim h9
           have h11 : ¬ (Q) := h10
           exact h11
      | inr h12 =>
           have h13 : False := h1 h12
           have h14 : ¬ (Q) := False.elim h13
           exact h14
    exact h15
  exact h16

end
