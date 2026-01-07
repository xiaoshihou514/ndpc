-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (P : U → Prop)


example {c: U}
  (h1 : ∃ x : U, (¬ ((P x))))
: ¬ (∀ x : U, ((P x))) := by
  have h9 : ¬ (∀ x : U, ((P x))) := by
    rcases h1 with ⟨c, h2⟩
    have h7 : ¬ (∀ x : U, ((P x))) := by
      intro h3
      have h4 : (P c) := h3 c
      have h5 : False := h2 h4
      have h6 : False := h5
      exact h6
    have h8 : ¬ (∀ x : U, ((P x))) := h7
    exact h8
  exact h9

end
