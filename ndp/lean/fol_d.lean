-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (P : U → Prop)
variable (c : Prop)


example {c: U}
  (h1 : ¬ (∀ x : U, ((P x))))
: ∃ x : U, (¬ ((P x))) := by
  have h10 : ∃ x : U, (¬ ((P x))) := by
    apply byContradiction
    intro h2
    have h8 : ∀ x : U, ((P x)) := by
      intro c
      have h7 : (P c) := by
        apply byContradiction
        intro h4
        have h5 : ∃ x : U, (¬ ((P x))) := Exists.intro c h4
        have h6 : False := h2 h5
        exact h6
      exact h7
    have h9 : False := h1 h8
    exact h9
  exact h10

end
