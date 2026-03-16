-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (P : U → Prop)
variable (c : Prop)


example {c: U}
  (h1 : ¬ (∃ x : U, ((P x))))
: ∀ x : U, (¬ ((P x))) := by
  have h7 : ∀ x : U, (¬ ((P x))) := by
    intro c
    have h6 : ¬ ((P c)) := by
      intro h3
      have h4 : ∃ x : U, ((P x)) := Exists.intro c h3
      have h5 : False := h1 h4
      exact h5
    exact h6
  exact h7

end
