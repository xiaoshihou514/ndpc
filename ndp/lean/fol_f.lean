-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom F : Prop → Prop
axiom G : Prop → Prop

example {x c d : Prop}
  (h1 : (∃ x : Prop, (F x)) ∨ (∃ x : Prop, (G x)))
: ∃ x : Prop, ((F x) ∨ (G x)) := by
  cases h1 with
 inl h2 =>
       have h12 : ∃ x : Prop, ((F x) ∨ (G x)) := by
         rcases h8 with ⟨x, h9⟩
         have h10 : (F d) ∨ (G d) := Or.inr 9
         have h11 : ∃ x : Prop, ((F x) ∨ (G x)) := Exists.intro x 10
         exact h11
       have h13 : ∃ x : Prop, ((F x) ∨ (G x)) := h12
       exact h7
 inr h8 =>
  
       exact h13
  exact h14

end
