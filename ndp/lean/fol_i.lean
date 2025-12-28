-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom P : Prop → Prop

example {a b c : Prop}
  (h1 : (a = b) ∨ (a = c))
  (h2 : (a = b) ∨ (c = b))
  (h3 : (P a) ∨ (P b))
: (P a) ∧ (P b) := by
  cases h1 with
 inl h4 =>
       cases h2 with
   inl h7 =>
            have h10 : a = b := Eq.subst 9 6
            exact h8
   inr h9 =>
       
            exact h10
       exact h5
 inr h6 =>
  
       exact h11
  cases h3 with
 inl h13 =>
       have h20 : P a := Eq.subst 12 17
       have h21 : (P a) ∧ (P b) := And.Intro 18 17
       exact h16
 inr h17 =>
  
       exact h19
  exact h20

end
