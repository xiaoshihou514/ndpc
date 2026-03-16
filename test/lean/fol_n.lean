-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (Q : U → Prop)
variable (a : Prop)
variable (b : Prop)
variable (P : U → Prop)


example {a b: U}
  (h1 : ∀ x : U, (∀ y : U, (((P x)) → ((Q y)))))
: ∀ x : U, (((P x)) → (∀ z : U, ((Q z)))) := by
  have h10 : ∀ x : U, (((P x)) → (∀ z : U, ((Q z)))) := by
    intro a
    have h3 : ∀ y : U, (((P a)) → ((Q y))) := h1 a
    have h9 : ((P a)) → (∀ z : U, ((Q z))) := by
      intro h4
      have h8 : ∀ z : U, ((Q z)) := by
        intro b
        have h6 : ((P a)) → ((Q b)) := h3 b
        have h7 : (Q b) := h6 h4
        exact h7
      exact h8
    exact h9
  exact h10

end
