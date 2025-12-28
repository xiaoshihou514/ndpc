-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom even : Prop → Prop
axiom odd : Prop → Prop

example {n a : Prop}
  (h1 : ∀ n : Prop, ((¬ (even n)) → (odd n)))
  (h2 : ∀ n : Prop, ((odd n) → (¬ (even n))))
: ∀ n : Prop, (¬ ((even n) ∧ (odd n))) := by
  have h11 : ∀ n : Prop, (¬ ((even n) ∧ (odd n))) := by
    intro h3
    have h4 : (odd a) → (¬ (even a)) := h2 n
    have h10 : ¬ ((even a) ∧ (odd a)) := by
      intro h5
      have h6 : odd a := And.right 5
      have h7 : ¬ (even a) := h6 h4
      have h8 : even a := And.left 5
      have h9 : False := h7 h8
      exact h9
    exact h9
  exact h11

end
