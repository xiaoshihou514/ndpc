-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom F : Prop → Prop
axiom G : Prop → Prop

example {x c : Prop}
  (h1 : ∃ x : Prop, ((F x) ∨ (G x)))
: (∃ x : Prop, (F x)) ∨ (∃ x : Prop, (G x)) := by
  have h15 : (∃ x : Prop, (F x)) ∨ (∃ x : Prop, (G x)) := by
    rcases h1 with ⟨x, h2⟩
    cases h2 with
 inl h3 =>
         have h8 : ∃ x : Prop, (G x) := Exists.intro x 7
         have h9 : (∃ x : Prop, (F x)) ∨ (∃ x : Prop, (G x)) := Or.inr 8
         exact h6
 inr h7 =>
    
         exact h9
    exact h10
  exact h11

end
