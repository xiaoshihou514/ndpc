-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (G : U → Prop)
variable (F : U → Prop)
variable (c2 : Prop)
variable (c1 : Prop)


example {c2 c1: U}
  (h1 : ∀ x : U, (((F x)) ∧ ((G x))))
: (∀ x : U, ((F x))) ∧ (∀ x : U, ((G x))) := by
  have h5 : ∀ x : U, ((F x)) := by
    intro c1
    have h3 : ((F c1)) ∧ ((G c1)) := h1 c1
    have h4 : (F c1) := And.left h3
    exact h4
  have h9 : ∀ x : U, ((G x)) := by
    intro c2
    have h7 : ((F c2)) ∧ ((G c2)) := h1 c2
    have h8 : (G c2) := And.right h7
    exact h8
  have h10 : (∀ x : U, ((F x))) ∧ (∀ x : U, ((G x))) := And.intro h5 h9
  exact h10

end
