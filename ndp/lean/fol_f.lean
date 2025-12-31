-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom F : Prop → Prop
axiom G : Prop → Prop

example {x c d : Prop}
  (h1 : (∃ x : Prop, ((F x))) ∨ (∃ x : Prop, ((G x))))
: ∃ x : Prop, (((F x)) ∨ ((G x))) := by
  have h14 : ∃ x : Prop, (((F x)) ∨ ((G x))) := by
    cases h1 with
    | inl h2 =>
         have h6 : ∃ x : Prop, (((F x)) ∨ ((G x))) := by
           rcases h2 with ⟨c, h3⟩
           have h4 : ((F c)) ∨ ((G c)) := Or.inl h3
           have h5 : ∃ x : Prop, (((F x)) ∨ ((G x))) := Exists.intro c h4
           exact h5
         have h7 : ∃ x : Prop, (((F x)) ∨ ((G x))) := h6
         exact h7
    | inr h8 =>
         have h12 : ∃ x : Prop, (((F x)) ∨ ((G x))) := by
           rcases h8 with ⟨d, h9⟩
           have h10 : ((F d)) ∨ ((G d)) := Or.inr h9
           have h11 : ∃ x : Prop, (((F x)) ∨ ((G x))) := Exists.intro d h10
           exact h11
         have h13 : ∃ x : Prop, (((F x)) ∨ ((G x))) := h12
         exact h13
  exact h14

end
