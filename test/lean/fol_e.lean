-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (F : U → Prop)
variable (G : U → Prop)


example {c: U}
  (h1 : ∃ x : U, (((F x)) ∨ ((G x))))
: (∃ x : U, ((F x))) ∨ (∃ x : U, ((G x))) := by
  have h11 : (∃ x : U, ((F x))) ∨ (∃ x : U, ((G x))) := by
    rcases h1 with ⟨c, h2⟩
    have h10 : (∃ x : U, ((F x))) ∨ (∃ x : U, ((G x))) := by
      cases h2 with
      | inl h3 =>
           have h4 : ∃ x : U, ((F x)) := Exists.intro c h3
           have h5 : (∃ x : U, ((F x))) ∨ (∃ x : U, ((G x))) := Or.inl h4
           have h6 : (∃ x : U, ((F x))) ∨ (∃ x : U, ((G x))) := h5
           exact h6
      | inr h7 =>
           have h8 : ∃ x : U, ((G x)) := Exists.intro c h7
           have h9 : (∃ x : U, ((F x))) ∨ (∃ x : U, ((G x))) := Or.inr h8
           exact h9
    exact h10
  exact h11

end
