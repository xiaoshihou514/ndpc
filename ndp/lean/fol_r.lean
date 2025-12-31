-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom even : Prop → Prop
axiom odd : Prop → Prop

example {n c : Prop}
  (h1 : ∀ n : Prop, ((¬ ((even n))) → ((odd n))))
  (h2 : ∀ n : Prop, ((¬ ((odd n))) → ((even n))))
: ∀ n : Prop, (((even n)) ∨ ((odd n))) := by
  have h14 : ∀ n : Prop, (((even n)) ∨ ((odd n))) := by
    intro c
    have h4 : (¬ ((even c))) → ((odd c)) := h1 c
    have h5 : (¬ ((odd c))) → ((even c)) := h2 c
    have h6 : ((odd c)) ∨ (¬ ((odd c))) := em (odd c)
    have h13 : ((even c)) ∨ ((odd c)) := by
      cases h6 with
      | inl h7 =>
           have h8 : ((even c)) ∨ ((odd c)) := Or.inr h7
           have h9 : ((even c)) ∨ ((odd c)) := h8
           exact h9
      | inr h10 =>
           have h11 : (even c) := h5 h10
           have h12 : ((even c)) ∨ ((odd c)) := Or.inl h11
           exact h12
    exact h13
  exact h14

end
