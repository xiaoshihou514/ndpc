-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (F : U → Prop)
variable (G : U → Prop)
variable (c : Prop)


example {c: U}
  (h1 : (∀ x : U, ((F x))) ∧ (∀ x : U, ((G x))))
: ∀ x : U, (((F x)) ∧ ((G x))) := by
  have h8 : ∀ x : U, (((F x)) ∧ ((G x))) := by
    intro c
    have h3 : ∀ x : U, ((F x)) := And.left h1
    have h4 : (F c) := h3 c
    have h5 : ∀ x : U, ((G x)) := And.right h1
    have h6 : (G c) := h5 c
    have h7 : ((F c)) ∧ ((G c)) := And.intro h4 h6
    exact h7
  exact h8

end
