-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom P : Prop → Prop

example {x c : Prop}
  (h1 : ¬ (∀ x : Prop, (P x)))
: ∃ x : Prop, (¬ (P x)) := by
  have h10 : ∃ x : Prop, (¬ (P x)) := by
    apply byContradiction
    intro hh2
    have h8 : ∀ x : Prop, (P x) := by
      intro h3
      have h7 : P c := by
        apply byContradiction
        intro hh4
        have h5 : ∃ x : Prop, (¬ (P x)) := Exists.intro x 4
        have h6 : False := h2 h5
        exact h6
      exact h7
    have h9 : False := h1 h8
    exact h9
  exact h10

end
