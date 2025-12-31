-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom P : Prop → Prop

example {x c : Prop}
  (h1 : ¬ (∃ x : Prop, ((P x))))
: ∀ x : Prop, (¬ ((P x))) := by
  have h7 : ∀ x : Prop, (¬ ((P x))) := by
    intro c
    have h6 : ¬ ((P c)) := by
      intro h3
      have h4 : ∃ x : Prop, ((P x)) := Exists.intro c h3
      have h5 : False := h1 h4
      exact h5
    exact h6
  exact h7

end
