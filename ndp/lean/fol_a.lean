-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom P : Prop → Prop

example {x c : Prop}
  (h1 : ∀ x : Prop, (¬ ((P x))))
: ¬ (∃ x : Prop, ((P x))) := by
  have h9 : ¬ (∃ x : Prop, ((P x))) := by
    intro h2
    have h7 : False := by
      rcases h2 with ⟨c, h3⟩
      have h4 : ¬ ((P c)) := h1 c
      have h5 : False := h4 h3
      have h6 : False := h5
      exact h6
    have h8 : False := h7
    exact h8
  exact h9

end
