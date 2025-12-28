-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {M I L B P W : Prop}
  (h1 : ¬ (P))
  (h2 : ((B) ∨ (W)) → (P))
  (h3 : (¬ (I)) → (B))
  (h4 : (¬ (W)) → (M))
  (h5 : (L) → ((¬ (I)) ∧ (¬ (M))))
: ¬ (L) := by
  have h13 : ¬ (L) := by
    intro h6
    have h7 : (¬ (I)) ∧ (¬ (M)) := h6 h5
    have h8 : ¬ (I) := And.left 7
    have h9 : B := h8 h3
    have h10 : (B) ∨ (W) := Or.inl 9
    have h11 : P := h10 h2
    have h12 : False := h1 h11
    exact h12
  exact h13

end
