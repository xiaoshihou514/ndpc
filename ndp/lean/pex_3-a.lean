-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {P Q R S : Prop}
  (h1 : (P) → (Q))
  (h2 : (¬ (P)) → (R))
  (h3 : (Q) → (S))
  (h4 : (R) → (S))
: S := by
  have h5 : (P) ∨ (¬ (P)) := em P
  cases h5 with
 inl h6 =>
       have h11 : R := h10 h2
       have h12 : S := h11 h4
       exact h9
 inr h10 =>
  
       exact h12
  exact h13

end
