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
    intro h4
    have h5 : N := And.left h4
    have h6 : ¬ (t) := And.right h4
    have h11 : ¬ (C) := by
      intro h7
      have h8 : (C) ∧ (N) := And.intro h7 h5
      have h9 : t := h1 h8
      have h10 : False := h6 h9
      exact h10
    have h12 : ¬ (S) := And.right h2
    have h20 : ¬ ((S) ∨ (C)) := by
      intro h13
      have h19 : False := by
        cases h13 with
        | inl h14 =>
             have h15 : False := h12 h14
             have h16 : False := h15
             exact h16
        | inr h17 =>
             have h18 : False := h11 h17
             exact h18
      exact h19
    have h21 : H := And.left h2
    have h22 : (H) ∧ (¬ ((S) ∨ (C))) := And.intro h21 h20
    have h23 : P := h3 h22
    exact h23
  exact h24

end
