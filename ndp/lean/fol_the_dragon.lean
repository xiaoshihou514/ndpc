-- `lean *.lean` or https://live.lean-lang.org/
section
open Classical
set_option linter.unusedVariables false
axiom child : Prop → Prop → Prop
axiom fly : Prop → Prop
axiom green : Prop → Prop
axiom parent : Prop → Prop → Prop
axiom dragon : Prop → Prop
axiom happy : Prop → Prop

example {x y c z d : Prop}
  (h1 : ∀ x : Prop, (((∀ y : Prop, (((child y x)) → ((fly y)))) ∧ ((dragon x))) → ((happy x))))
  (h2 : ∀ x : Prop, ((((green x)) ∧ ((dragon x))) → ((fly x))))
  (h3 : ∀ x : Prop, ((∃ y : Prop, (((parent y x)) ∧ ((green y)))) → ((green x))))
  (h4 : ∀ z : Prop, (∀ x : Prop, ((((child x z)) ∧ ((dragon z))) → ((dragon x)))))
  (h5 : ∀ x : Prop, (∀ y : Prop, (((child y x)) → ((parent x y)))))
: ∀ x : Prop, (((dragon x)) → (((green x)) → ((happy x)))) := by
  have h27 : ∀ x : Prop, (((dragon x)) → (((green x)) → ((happy x)))) := by
    intro c
    have h26 : ((dragon c)) → (((green c)) → ((happy c))) := by
      intro h7
      have h25 : ((green c)) → ((happy c)) := by
        intro h8
        have h22 : ∀ y : Prop, (((child y c)) → ((fly y))) := by
          intro d
          have h21 : ((child d c)) → ((fly d)) := by
            intro h10
            have h11 : ∀ y : Prop, (((child y c)) → ((parent c y))) := h5 c
            have h12 : (parent c d) := h11 d h10
            have h13 : ((parent c d)) ∧ ((green c)) := And.intro h12 h8
            have h14 : ∃ y : Prop, (((parent y d)) ∧ ((green y))) := Exists.intro c h13
            have h15 : (green d) := h3 d h14
            have h16 : ((child d c)) ∧ ((dragon c)) := And.intro h10 h7
            have h17 : ∀ x : Prop, ((((child x c)) ∧ ((dragon c))) → ((dragon x))) := h4 c
            have h18 : (dragon d) := h17 d h16
            have h19 : ((green d)) ∧ ((dragon d)) := And.intro h15 h18
            have h20 : (fly d) := h2 d h19
            exact h20
          exact h21
        have h23 : (∀ y : Prop, (((child y c)) → ((fly y)))) ∧ ((dragon c)) := And.intro h22 h7
        have h24 : (happy c) := h1 c h23
        exact h24
      exact h25
    exact h26
  exact h27

end
