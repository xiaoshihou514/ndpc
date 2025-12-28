-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {R I f : Prop}
  (h1 : (R) → (¬ (I)))
  (h2 : (I) ∨ (f))
  (h3 : ¬ (f))
: ¬ (R) := by
  cases h2 with
 inl h4 =>
       have h9 : False := h3 h8
       have h10 : ¬ (R) := False.elim 9
       exact h7
 inr h8 =>
  
       exact h10
  exact h11

end
