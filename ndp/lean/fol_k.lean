-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom A : Prop → Prop
axiom B : Prop → Prop

example {x y c : Prop}
  (h1 : ∀ x : Prop, (((A x)) ∨ ((B x))))
  (h2 : ∀ y : Prop, (¬ ((A y))))
: ∀ x : Prop, ((B x)) := by
  have h13 : ∀ x : Prop, ((B x)) := by
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
