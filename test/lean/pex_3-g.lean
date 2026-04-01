-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (D : Prop)
variable (L : Prop)
variable (G : Prop)
variable (B : Prop)
variable (R : Prop)


example 
  (h1 : (R) → ((B) → ((D) ∨ (L))))
  (h2 : ¬ ((D) ∨ (G)))
  (h3 : ((L) ∨ (B)) → (G))
: (B) → (¬ (R)) := by
  have h10 : (B) → (¬ (R)) := by
    intro h4
    have h5 : (L) ∨ (B) := Or.inr h4
    have h6 : G := h3 h5
    have h7 : (D) ∨ (G) := Or.inr h6
    have h8 : False := h2 h7
    have h9 : ¬ (R) := False.elim h8
    exact h9
  exact h10

end
