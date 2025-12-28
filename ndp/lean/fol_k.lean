-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom A : Prop → Prop
axiom B : Prop → Prop

example {x y c : Prop}
  (h1 : ∀ x : Prop, ((A x) ∨ (B x)))
  (h2 : ∀ y : Prop, (¬ (A y)))
: ∀ x : Prop, (B x) := by
  have h15 : ∀ x : Prop, (B x) := by
    intro h3
    have h4 : (A c) ∨ (B c) := h1 x
    have h5 : ¬ (A c) := h2 y
    cases h4 with
 inl h6 =>
         have h11 : B c := h10
         exact h9
 inr h10 =>
    
         exact h11
    exact h13
  exact h13

end
