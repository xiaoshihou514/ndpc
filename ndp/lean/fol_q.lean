-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom likes : Prop → Prop → Prop

example {x John y Jack : Prop}
  (h1 : ∀ x : Prop, ((likes x John)))
  (h2 : ∀ y : Prop, (((likes John y)) → (y = Jack)))
: John = Jack := by
  have h8 : John = Jack := by
    apply byContradiction
    intro h3
    have h4 : (likes John John) := h1 John
    have h5 : ((likes John John)) → (John = Jack) := h2 John
    have h6 : John = Jack := h5 h4
    have h7 : False := h3 h6
    exact h7
  exact h8

end
