-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (P : U → Prop)


example {c: U}
  (h1 : ∀ x : U, (¬ ((P x))))
: ¬ (∃ x : U, ((P x))) := by
  have h9 : ¬ (∃ x : U, ((P x))) := by
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
