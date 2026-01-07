-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (P : U → Prop)
variable (Q : U → Prop)
variable (c : Prop)


example {c: U}
  (h1 : ∀ x : U, (((P x)) ∨ ((Q x))))
  (h2 : ¬ (∀ x : U, ((P x))))
: ¬ (∀ x : U, (¬ ((Q x)))) := by
  have h15 : ¬ (∀ x : U, (¬ ((Q x)))) := by
    intro h3
    have h13 : ∀ x : U, ((P x)) := by
      intro c
      have h5 : ((P c)) ∨ ((Q c)) := h1 c
      have h12 : (P c) := by
        cases h5 with
        | inl h6 =>
             have h7 : (P c) := h6
             exact h7
        | inr h8 =>
             have h9 : ¬ ((Q c)) := h3 c
             have h10 : False := h9 h8
             have h11 : (P c) := False.elim h10
             exact h11
      exact h12
    have h14 : False := h2 h13
    exact h14
  exact h15

end
