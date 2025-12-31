-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom F : Prop → Prop
axiom G : Prop → Prop

example {x c : Prop}
  (h1 : ∃ x : Prop, (((F x)) ∨ ((G x))))
: (∃ x : Prop, ((F x))) ∨ (∃ x : Prop, ((G x))) := by
  have h11 : (∃ x : Prop, ((F x))) ∨ (∃ x : Prop, ((G x))) := by
    rcases h1 with ⟨c, h2⟩
    have h10 : (∃ x : Prop, ((F x))) ∨ (∃ x : Prop, ((G x))) := by
      cases h2 with
      | inl h3 =>
           have h4 : ∃ x : Prop, ((F x)) := Exists.intro c h3
           have h5 : (∃ x : Prop, ((F x))) ∨ (∃ x : Prop, ((G x))) := Or.inl h4
           have h6 : (∃ x : Prop, ((F x))) ∨ (∃ x : Prop, ((G x))) := h5
           exact h6
      | inr h7 =>
           have h8 : ∃ x : Prop, ((G x)) := Exists.intro c h7
           have h9 : (∃ x : Prop, ((F x))) ∨ (∃ x : Prop, ((G x))) := Or.inr h8
           exact h9
    exact h10
  exact h11

end
