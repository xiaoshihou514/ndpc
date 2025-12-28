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
  (h1 : ∀ x : Prop, (((∀ y : Prop, ((child y x) → (fly y))) ∧ (dragon x)) → (happy x)))
  (h2 : ∀ x : Prop, (((green x) ∧ (dragon x)) → (fly x)))
  (h3 : ∀ x : Prop, ((∃ y : Prop, ((parent y x) ∧ (green y))) → (green x)))
  (h4 : ∀ z : Prop, (∀ x : Prop, (((child x z) ∧ (dragon z)) → (dragon x))))
  (h5 : ∀ x : Prop, (∀ y : Prop, ((child y x) → (parent x y))))
: ∀ x : Prop, ((dragon x) → ((green x) → (happy x))) := by
  have h27 : ∀ x : Prop, ((dragon x) → ((green x) → (happy x))) := by
    intro h6
    have h26 : (dragon c) → ((green c) → (happy c)) := by
      have h25 : (green c) → (happy c) := by
        have h22 : ∀ y : Prop, ((child y c) → (fly y)) := by
          intro h9
          have h21 : (child d c) → (fly d) := by
            have h11 : ∀ y : Prop, ((child y c) → (parent c y)) := h5 x
            have h12 : parent c d := by
              intro hd
              exact h11
            have h13 : (parent c d) ∧ (green c) := And.Intro 12 8
            have h14 : ∃ y : Prop, ((parent y d) ∧ (green y)) := Exists.intro y 13
            have h15 : green d := by
              intro hd
              exact h3
            have h16 : (child d c) ∧ (dragon c) := And.Intro 10 7
            have h17 : ∀ x : Prop, (((child x c) ∧ (dragon c)) → (dragon x)) := h4 z
            have h18 : dragon d := by
              intro hd
              exact h17
            have h19 : (green d) ∧ (dragon d) := And.Intro 15 18
            have h20 : fly d := by
              intro hd
              exact h2
            exact h20
          exact h21
        have h23 : (∀ y : Prop, ((child y c) → (fly y))) ∧ (dragon c) := And.Intro 22 7
        have h24 : happy c := by
          intro hc
          exact h1
        exact h24
      exact h25
    exact h22
  exact h27

end
