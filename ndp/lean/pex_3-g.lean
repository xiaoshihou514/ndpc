-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {G L B R D : Prop}
  (h1 : (R) → ((B) → ((D) ∨ (L))))
  (h2 : ¬ ((D) ∨ (G)))
  (h3 : ((L) ∨ (B)) → (G))
: (B) → (¬ (R)) := by
  have h10 : (B) → (¬ (R)) := by
    have h5 : (L) ∨ (B) := Or.inr 4
    have h6 : G := h5 h3
    have h7 : (D) ∨ (G) := Or.inr 6
    have h8 : False := h2 h7
    have h9 : ¬ (R) := False.elim 8
    exact h7
  exact h10

end
