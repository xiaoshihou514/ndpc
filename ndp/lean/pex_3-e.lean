-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {G L B C D : Prop}
  (h1 : ((G) ∨ (B)) → (C))
  (h2 : (¬ (D)) → (¬ ((L) → (False))))
  (h3 : (C) → ((L) → (False)))
: (G) → ((B) → (D)) := by
  have h13 : (G) → ((B) → (D)) := by
    intro h4
    have h12 : (B) → (D) := by
      intro h5
      have h6 : (G) ∨ (B) := Or.inr h5
      have h7 : C := h1 h6
      have h8 : (L) → (False) := h3 h7
      have h9 : ¬ (¬ ((L) → (False))) := by
        intro tmp9
        exact tmp9 h8
      have h10 : ¬ (¬ (D)) := by
        intro tmp
        have htmp10 : ¬ ((L) → (False)) := h2 tmp
        exact h9 htmp10
      have h11 : D := by
        apply byContradiction
        intro tmp
        have htmp11 : False := h10 tmp
        contradiction
      exact h11
    exact h12
  exact h13

end
