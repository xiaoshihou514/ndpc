-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {D B C : Prop}
  (h1 : (D) ∨ (B))
  (h2 : ¬ ((D) ∨ (¬ (C))))
  (h3 : (B) → (C))
: C := by
  cases h1 with
 inl h4 =>
       have h10 : C := h9 h3
       exact h8
 inr h9 =>
  
       exact h10
  exact h11

end
