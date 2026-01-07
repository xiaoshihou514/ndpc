-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (even : U → Prop)
variable (odd : U → Prop)
variable (a : Prop)


example {a: U}
  (h1 : ∀ n : U, ((¬ ((even n))) → ((odd n))))
  (h2 : ∀ n : U, (((odd n)) → (¬ ((even n)))))
: ∀ n : U, (¬ (((even n)) ∧ ((odd n)))) := by
  have h11 : ∀ n : U, (¬ (((even n)) ∧ ((odd n)))) := by
    intro a
    have h4 : ((odd a)) → (¬ ((even a))) := h2 a
    have h10 : ¬ (((even a)) ∧ ((odd a))) := by
      intro h5
      have h6 : (odd a) := And.right h5
      have h7 : ¬ ((even a)) := h4 h6
      have h8 : (even a) := And.left h5
      have h9 : False := h7 h8
      exact h9
    exact h10
  exact h11

end
