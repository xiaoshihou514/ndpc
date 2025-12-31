-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom F : Prop → Prop
axiom G : Prop → Prop

example {x c1 c2 : Prop}
  (h1 : ∀ x : Prop, (((F x)) ∧ ((G x))))
: (∀ x : Prop, ((F x))) ∧ (∀ x : Prop, ((G x))) := by
  have h5 : ∀ x : Prop, ((F x)) := by
    intro c1
    have h3 : ((F c1)) ∧ ((G c1)) := h1 c1
    have h4 : (F c1) := And.left h3
    exact h4
  have h9 : ∀ x : Prop, ((G x)) := by
    intro c2
    have h7 : ((F c2)) ∧ ((G c2)) := h1 c2
    have h8 : (G c2) := And.right h7
    exact h8
  have h10 : (∀ x : Prop, ((F x))) ∧ (∀ x : Prop, ((G x))) := And.intro h5 h9
  exact h10

end
