-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {D B C : Prop}
  (h1 : (D) ∨ (B))
  (h2 : ¬ ((D) ∨ (¬ (C))))
  (h3 : (B) → (C))
: C := by
  have h11 : C := by
    cases h1 with
    | inl h4 =>
         have h5 : (D) ∨ (¬ (C)) := Or.inl h4
         have h6 : False := h2 h5
         have h7 : C := False.elim h6
         have h8 : C := h7
         exact h8
    | inr h9 =>
         have h10 : C := h3 h9
         exact h10
  exact h11

end
