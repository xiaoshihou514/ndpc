-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (A : U → Prop)
variable (B : U → Prop)
variable (C : U → Prop)


example {a: U}
  (h1 : ∀ x : U, ((((A x)) ∧ ((B x))) → ((C x))))
  (h2 : ∃ x : U, (((A x)) ∧ ((B x))))
: ∃ x : U, (((A x)) ∧ ((C x))) := by
  have h9 : ∃ x : U, (((A x)) ∧ ((C x))) := by
    rcases h2 with ⟨a, h3⟩
    have h4 : (((A a)) ∧ ((B a))) → ((C a)) := h1 a
    have h5 : (C a) := h4 h3
    have h6 : (A a) := And.left h3
    have h7 : ((A a)) ∧ ((C a)) := And.intro h6 h5
    have h8 : ∃ x : U, (((A x)) ∧ ((C x))) := Exists.intro a h7
    exact h8
  exact h9

end
