-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom A : Prop → Prop
axiom B : Prop → Prop
axiom C : Prop → Prop

example {x a : Prop}
  (h1 : ∀ x : Prop, ((((A x)) ∧ ((B x))) → ((C x))))
  (h2 : ∃ x : Prop, (((A x)) ∧ ((B x))))
: ∃ x : Prop, (((A x)) ∧ ((C x))) := by
  have h9 : ∃ x : Prop, (((A x)) ∧ ((C x))) := by
    rcases h2 with ⟨a, h3⟩
    have h4 : (((A a)) ∧ ((B a))) → ((C a)) := h1 a
    have h5 : (C a) := h4 h3
    have h6 : (A a) := And.left h3
    have h7 : ((A a)) ∧ ((C a)) := And.intro h6 h5
    have h8 : ∃ x : Prop, (((A x)) ∧ ((C x))) := Exists.intro a h7
    exact h8
  exact h9

end
