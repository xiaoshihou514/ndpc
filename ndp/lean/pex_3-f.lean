-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (L : Prop)
variable (I : Prop)
variable (M : Prop)
variable (B : Prop)
variable (W : Prop)
variable (P : Prop)


example 
  (h1 : ¬ (P))
  (h2 : ((B) ∨ (W)) → (P))
  (h3 : (¬ (I)) → (B))
  (h4 : (¬ (W)) → (M))
  (h5 : (L) → ((¬ (I)) ∧ (¬ (M))))
: ¬ (L) := by
  have h13 : ¬ (L) := by
    intro h6
    have h7 : (¬ (I)) ∧ (¬ (M)) := h5 h6
    have h8 : ¬ (I) := And.left h7
    have h9 : B := h3 h8
    have h10 : (B) ∨ (W) := Or.inl h9
    have h11 : P := h2 h10
    have h12 : False := h1 h11
    exact h12
  exact h13

end
