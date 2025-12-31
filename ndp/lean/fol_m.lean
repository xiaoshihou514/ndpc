-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom R : Prop → Prop → Prop

example {c y x : Prop}

: ∃ x : Prop, (∃ y : Prop, (((R x y)) → ((R y x)))) := by
  have h3 : ((R c c)) → ((R c c)) := by
    intro h1
    have h2 : (R c c) := h1
    exact h2
  have h4 : ∃ y : Prop, (((R c y)) → ((R y c))) := Exists.intro c h3
  have h5 : ∃ x : Prop, (∃ y : Prop, (((R x y)) → ((R y x)))) := Exists.intro c h4
  exact h5

end
