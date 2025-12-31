-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom P : Prop → Prop
axiom Q : Prop → Prop

example {x y a b z : Prop}
  (h1 : ∀ x : Prop, (∀ y : Prop, (((P x)) → ((Q y)))))
: ∀ x : Prop, (((P x)) → (∀ z : Prop, ((Q z)))) := by
  have h10 : ∀ x : Prop, (((P x)) → (∀ z : Prop, ((Q z)))) := by
    intro a
    have h3 : ∀ y : Prop, (((P a)) → ((Q y))) := h1 a
    have h9 : ((P a)) → (∀ z : Prop, ((Q z))) := by
      intro h4
      have h8 : ∀ z : Prop, ((Q z)) := by
        intro b
        have h6 : ((P a)) → ((Q b)) := h3 b
        have h7 : (Q b) := h6 h4
        exact h7
      exact h8
    exact h9
  exact h10

end
