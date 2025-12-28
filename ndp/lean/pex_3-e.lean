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
    have h12 : (B) → (D) := by
      have h6 : (G) ∨ (B) := Or.inr 5
      have h7 : C := h6 h1
      have h8 : (L) → (False) := h7 h3
      have h9 : ¬ (¬ ((L) → (False))) := by
        intro hp9
        have hp : False := hp9 h8
      have h10 : ¬ (¬ (D)) := by
        intro hA
        have hB : ¬ ((L) → (False)) := h2 hA
        exact h9 hB
      have h11 : D := by
        apply byContradiction
        intro h
        have h_ : False := h h1
        contradiction
      exact h11
    exact h10
  exact h13

end
