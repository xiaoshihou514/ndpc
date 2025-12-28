-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom F : Prop → Prop
axiom G : Prop → Prop

example {x c : Prop}
  (h1 : (∀ x : Prop, (F x)) ∧ (∀ x : Prop, (G x)))
: ∀ x : Prop, ((F x) ∧ (G x)) := by
  have h8 : ∀ x : Prop, ((F x) ∧ (G x)) := by
    intro h2
    have h3 : ∀ x : Prop, (F x) := And.left 1
    have h4 : F c := h3 x
    have h5 : ∀ x : Prop, (G x) := And.right 1
    have h6 : G c := h5 x
    have h7 : (F c) ∧ (G c) := And.Intro 4 6
    exact h7
  exact h8

end
