-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (A : U → Prop)
variable (B : U → Prop)
variable (c : Prop)


example {c: U}
  (h1 : ∀ x : U, (((A x)) ∨ ((B x))))
  (h2 : ∀ y : U, (¬ ((A y))))
: ∀ x : U, ((B x)) := by
  have h13 : ∀ x : U, ((B x)) := by
    intro c
    have h4 : ((A c)) ∨ ((B c)) := h1 c
    have h5 : ¬ ((A c)) := h2 c
    have h12 : (B c) := by
      cases h4 with
      | inl h6 =>
           have h7 : False := h5 h6
           have h8 : (B c) := False.elim h7
           have h9 : (B c) := h8
           exact h9
      | inr h10 =>
           have h11 : (B c) := h10
           exact h11
    exact h12
  exact h13

end
