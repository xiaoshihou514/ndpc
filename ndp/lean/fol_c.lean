-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom P : Prop → Prop

example {x c : Prop}
  (h1 : ∃ x : Prop, (¬ ((P x))))
: ¬ (∀ x : Prop, ((P x))) := by
  have h9 : ¬ (∀ x : Prop, ((P x))) := by
    rcases h1 with ⟨c, h2⟩
    have h7 : ¬ (∀ x : Prop, ((P x))) := by
      intro h3
      have h4 : (P c) := h3 c
      have h5 : False := h2 h4
      have h6 : False := h5
      exact h6
    have h8 : ¬ (∀ x : Prop, ((P x))) := h7
    exact h8
  exact h9

end
