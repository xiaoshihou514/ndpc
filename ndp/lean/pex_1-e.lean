-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {p q : Prop}
  (h1 : p)
: ((p) ∧ (q)) ∨ ((p) ∧ (¬ (q))) := by
  have h2 : (q) ∨ (¬ (q)) := em q
  cases h2 with
 inl h3 =>
       have h8 : (p) ∧ (¬ (q)) := And.Intro 1 7
       have h9 : ((p) ∧ (q)) ∨ ((p) ∧ (¬ (q))) := Or.inr 8
       exact h6
 inr h7 =>
  
       exact h9
  exact h10

end
