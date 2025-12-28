-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {N t P C H S : Prop}
  (h1 : ((C) ∧ (N)) → (t))
  (h2 : (H) ∧ (¬ (S)))
  (h3 : ((H) ∧ (¬ ((S) ∨ (C)))) → (P))
: ((N) ∧ (¬ (t))) → (P) := by
  have h24 : ((N) ∧ (¬ (t))) → (P) := by
    have h5 : N := And.left 4
    have h6 : ¬ (t) := And.right 4
    have h11 : ¬ (C) := by
      intro h7
      have h8 : (C) ∧ (N) := And.Intro 7 5
      have h9 : t := h8 h1
      have h10 : False := h6 h9
      exact h10
    have h12 : ¬ (S) := And.right 2
    have h20 : ¬ ((S) ∨ (C)) := by
      intro h13
      cases h13 with
 inl h14 =>
           have h18 : False := h11 h17
           exact h16
 inr h17 =>
      
           exact h18
      exact h19
    have h21 : H := And.left 2
    have h22 : (H) ∧ (¬ ((S) ∨ (C))) := And.Intro 21 20
    have h23 : P := h22 h3
    exact h21
  exact h24

end
