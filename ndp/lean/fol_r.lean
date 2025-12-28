-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom even : Prop → Prop
axiom odd : Prop → Prop

example {n c : Prop}
  (h1 : ∀ n : Prop, ((¬ (even n)) → (odd n)))
  (h2 : ∀ n : Prop, ((¬ (odd n)) → (even n)))
: ∀ n : Prop, ((even n) ∨ (odd n)) := by
  have h14 : ∀ n : Prop, ((even n) ∨ (odd n)) := by
    intro h3
    have h4 : (¬ (even c)) → (odd c) := h1 n
    have h5 : (¬ (odd c)) → (even c) := h2 n
    have h6 : (odd c) ∨ (¬ (odd c)) := em odd c
    cases h6 with
 inl h7 =>
         have h11 : even c := h10 h5
         have h12 : (even c) ∨ (odd c) := Or.inl 11
         exact h9
 inr h10 =>
    
         exact h12
    exact h12
  exact h14

end
