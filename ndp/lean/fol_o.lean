-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom P : Prop → Prop
axiom Q : Prop → Prop

example {x c : Prop}
  (h1 : ∀ x : Prop, (((P x)) ∨ ((Q x))))
  (h2 : ¬ (∀ x : Prop, ((P x))))
: ¬ (∀ x : Prop, (¬ ((Q x)))) := by
  have h15 : ¬ (∀ x : Prop, (¬ ((Q x)))) := by
    intro h3
    have h13 : ∀ x : Prop, ((P x)) := by
      intro c
      have h5 : ((P c)) ∨ ((Q c)) := h1 c
      have h12 : (P c) := by
        cases h5 with
        | inl h6 =>
             have h7 : (P c) := h6
             exact h7
        | inr h8 =>
             have h9 : ¬ ((Q c)) := h3 c
             have h10 : False := h9 h8
             have h11 : (P c) := False.elim h10
             exact h11
      exact h12
    have h14 : False := h2 h13
    exact h14
  exact h15

end
