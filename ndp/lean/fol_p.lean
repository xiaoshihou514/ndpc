-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false


example {a b c : Prop}
  (h1 : a = b)
  (h2 : ¬ ((b = b) ∧ (b = c)))
: ¬ (a = c) := by
  have h9 : ¬ (a = c) := by
    intro h3
    have h4 : c = b := Eq.subst 3 1
    have h5 : b = c := Eq.symm 4
    have h6 : b = b := rfl
    have h7 : (b = b) ∧ (b = c) := And.Intro 6 5
    have h8 : False := h2 h7
    exact h8
  exact h9

end
