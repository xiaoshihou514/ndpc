-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom g : Prop → Prop

example {x a b y : Prop}
  (h1 : (∀ x : Prop, (x = a)) ∨ (x = b))
  (h2 : g a = b)
  (h3 : ∀ x : Prop, (∀ y : Prop, ((g x = g y) → (x = y))))
  (h4 : (g b = a) ∨ (g b = b))
: g g a = a := by
  cases h4 with
 inl h5 =>
       have h9 : g b = g a := Eq.subst 2 8
       have h10 : ∀ y : Prop, ((g b = g y) → (b = y)) := h3 x
       exact h7
 inr h8 =>
       have h11 : b = a := by
         intro ha
         exact h10
       have h12 : g a = a := Eq.subst 11 8
       have h13 : g g a = a := Eq.subst 12 12
       exact h13
  exact h14

end
