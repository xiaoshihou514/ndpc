-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
variable (U : Type)
variable (P : U → Prop)


example {a b c: U}
  (h1 : (a = b) ∨ (a = c))
  (h2 : (a = b) ∨ (c = b))
  (h3 : ((P a)) ∨ ((P b)))
: ((P a)) ∧ ((P b)) := by
  have h12 : a = b := by
    cases h1 with
    | inl h4 =>
         have h5 : a = b := h4
         exact h5
    | inr h6 =>
         have h11 : a = b := by
           cases h2 with
           | inl h7 =>
                have h8 : a = b := h7
                exact h8
           | inr h9 =>
                have h10 : a = b := by
                  rw [← h9]
                  exact h6
                exact h10
         exact h11
  have h20 : ((P a)) ∧ ((P b)) := by
    cases h3 with
    | inl h13 =>
         have h14 : (P b) := by
           rw [← h12]
           exact h13
         have h15 : ((P a)) ∧ ((P b)) := And.intro h13 h14
         have h16 : ((P a)) ∧ ((P b)) := h15
         exact h16
    | inr h17 =>
         have h18 : (P a) := by
           rw [h12]
           exact h17
         have h19 : ((P a)) ∧ ((P b)) := And.intro h18 h17
         exact h19
  exact h20

end
