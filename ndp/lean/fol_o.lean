-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom P : Prop → Prop
axiom Q : Prop → Prop

example {x c : Prop}
  (h1 : ∀ x : Prop, ((P x) ∨ (Q x)))
  (h2 : ¬ (∀ x : Prop, (P x)))
: ¬ (∀ x : Prop, (¬ (Q x))) := by
  have h16 : ¬ (∀ x : Prop, (¬ (Q x))) := by
    intro h3
    have h14 : ∀ x : Prop, (P x) := by
      intro h4
      have h5 : (P c) ∨ (Q c) := h1 x
      cases h5 with
 inl h6 =>
           have h9 : ¬ (Q c) := h3 x
           exact h7
 inr h8 =>
           have h10 : False := h9 h8
           have h11 : P c := False.elim 10
           exact h11
      exact h13
    have h15 : False := h2 h13
    exact h14
  exact h15

end
