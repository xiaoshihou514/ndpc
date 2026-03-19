package ndpc.web.components

object Examples:
    val default: String =
        """|p ^ q [premise]
           |p [^E(1)]
           |q [^E(1)]
           |q ^ p [^I(3, 2)]""".stripMargin

    val all: List[(String, String)] = List(
      "∧ intro/elim" -> default,
      "Law of Excluded Middle" ->
          """|T [ass] -- no premises
             |p / ~p [LEM]""".stripMargin,

      // ── Propositional logic ──────────────────────────────────────────

      "Contrapositive" ->
          // (p → q) → (¬q → ¬p)
          // ->E(implication, antecedent)   ~E(negation, positive)
          """|  p -> q  [ass]
             |    ~q  [ass]
             |      p  [ass]
             |      q  [->E(1, 3)]
             |      F  [~E(2, 4)]
             |    ~p  [~I(3, 5)]
             |  ~q -> ~p  [->I(2, 6)]
             |(p -> q) -> (~q -> ~p)  [->I(1, 7)]""".stripMargin,
      "Peirce's Law" ->
          // ((p → q) → p) → p   — classical tautology, requires LEM
          """|  (p -> q) -> p  [ass]
             |  p / ~p  [LEM]
             |    p  [ass]
             |    p  [tick(3)]
             |    ~p  [ass]
             |      p  [ass]
             |      F  [~E(5, 6)]
             |      q  [FE(7)]
             |    p -> q  [->I(6, 8)]
             |    p  [->E(1, 9)]
             |  p  [/E(2, 3, 4, 5, 10)]
             |((p -> q) -> p) -> p  [->I(1, 11)]""".stripMargin,
      "De Morgan (¬∨)" ->
          // ¬(p ∨ q) → ¬p ∧ ¬q
          """|  ~(p / q)  [ass]
             |    p  [ass]
             |    p / q  [/I(2)]
             |    F  [~E(1, 3)]
             |  ~p  [~I(2, 4)]
             |    q  [ass]
             |    p / q  [/I(6)]
             |    F  [~E(1, 7)]
             |  ~q  [~I(6, 8)]
             |  ~p ^ ~q  [^I(5, 9)]
             |~(p / q) -> ~p ^ ~q  [->I(1, 10)]""".stripMargin,
      "Hypothetical syllogism" ->
          // (p → q) ∧ (q → r) → (p → r)   — transitivity of implication
          """|  (p -> q) ^ (q -> r)  [ass]
             |  p -> q  [^E(1)]
             |  q -> r  [^E(1)]
             |    p  [ass]
             |    q  [->E(2, 4)]
             |    r  [->E(3, 5)]
             |  p -> r  [->I(4, 6)]
             |((p -> q) ^ (q -> r)) -> (p -> r)  [->I(1, 7)]""".stripMargin,
      "Disjunctive syllogism" ->
          // (p ∨ q) ∧ ¬p → q
          """|  (p / q) ^ ~p  [ass]
             |  p / q  [^E(1)]
             |  ~p  [^E(1)]
             |    p  [ass]
             |    F  [~E(3, 4)]
             |    q  [FE(5)]
             |    q  [tick(6)]
             |    q  [ass]
             |    q  [tick(8)]
             |  q  [/E(2, 4, 7, 8, 9)]
             |(p / q) ^ ~p -> q  [->I(1, 10)]""".stripMargin,

      // ── First-order logic ────────────────────────────────────────────

      "Socrates is mortal" ->
          // Classic syllogism: ∀x.(human(x)→mortal(x)), human(socrates) ⊢ mortal(socrates)
          """|forall x. (human(x) -> mortal(x))  [premise]
             |human(socrates)  [premise]
             |mortal(socrates)  [forall->E(2, 1)]""".stripMargin,
      "∀ distributes over ∧" ->
          // (∀x.P(x)) ∧ (∀x.Q(x)) → ∀x.(P(x) ∧ Q(x))
          """|  (forall x. P(x)) ^ (forall x. Q(x))  [ass]
             |  forall x. P(x)  [^E(1)]
             |  forall x. Q(x)  [^E(1)]
             |    c  [forall I const]
             |    P(c)  [forallE(2)]
             |    Q(c)  [forallE(3)]
             |    P(c) ^ Q(c)  [^I(5, 6)]
             |  forall x. (P(x) ^ Q(x))  [forallI(4, 7)]
             |((forall x. P(x)) ^ (forall x. Q(x))) -> forall x. (P(x) ^ Q(x))  [->I(1, 8)]""".stripMargin,
      "∃ de Morgan (→)" ->
          // ∃x.¬P(x) → ¬∀x.P(x)
          """|  exists x. ~P(x)  [ass]
             |    ~P(c)  [ass]
             |      forall x. P(x)  [ass]
             |      P(c)  [forallE(3)]
             |      F  [~E(2, 4)]
             |    ~(forall x. P(x))  [~I(3, 5)]
             |    ~(forall x. P(x))  [tick(6)]
             |  ~(forall x. P(x))  [existsE(1, 2, 7)]
             |exists x. ~P(x) -> ~(forall x. P(x))  [->I(1, 8)]""".stripMargin,
      "∃ distributes over ∨" ->
          // ∃x.(F(x) ∨ G(x)) → (∃x.F(x)) ∨ (∃x.G(x))
          """|exists x. (F(x) / G(x))  [premise]
             |  F(c) / G(c)  [ass]
             |    F(c)  [ass]
             |    exists x. F(x)  [existsI(3)]
             |    exists x. F(x) / exists x. G(x)  [/I(4)]
             |    exists x. F(x) / exists x. G(x)  [tick(5)]
             |    G(c)  [ass]
             |    exists x. G(x)  [existsI(7)]
             |    exists x. F(x) / exists x. G(x)  [/I(8)]
             |  exists x. F(x) / exists x. G(x)  [/E(2, 3, 6, 7, 9)]
             |exists x. F(x) / exists x. G(x)  [existsE(1, 2, 10)]""".stripMargin,
      "Happy dragons" ->
          // All green dragons are happy — a predicate-logic benchmark
          """|forall x. (forall y. (child(y, x) -> fly(y)) ^ dragon(x) -> happy(x))  [premise]
             |forall x. (green(x) ^ dragon(x) -> fly(x))  [premise]
             |forall x. (exists y. (parent(y, x) ^ green(y)) -> green(x))  [premise]
             |forall z. forall x. (child(x, z) ^ dragon(z) -> dragon(x))  [premise]
             |forall x. forall y. (child(y, x) -> parent(x, y))  [premise]
             |  c  [forall I const]
             |    dragon(c)  [ass]
             |      green(c)  [ass]
             |        d  [forall I const]
             |          child(d, c)  [ass]
             |          forall y. (child(y, c) -> parent(c, y))  [forallE(5)]
             |          parent(c, d)  [forall->E(10, 11)]
             |          parent(c, d) ^ green(c)  [^I(12, 8)]
             |          exists y. (parent(y, d) ^ green(y))  [existsI(13)]
             |          green(d)  [forall->E(14, 3)]
             |          child(d, c) ^ dragon(c)  [^I(10, 7)]
             |          forall x. (child(x, c) ^ dragon(c) -> dragon(x))  [forallE(4)]
             |          dragon(d)  [forall->E(16, 17)]
             |          green(d) ^ dragon(d)  [^I(15, 18)]
             |          fly(d)  [forall->E(19, 2)]
             |        child(d, c) -> fly(d)  [->I(10, 20)]
             |      forall y. (child(y, c) -> fly(y))  [forallI(9, 21)]
             |      forall y. (child(y, c) -> fly(y)) ^ dragon(c)  [^I(22, 7)]
             |      happy(c)  [forall->E(23, 1)]
             |    green(c) -> happy(c)  [->I(8, 24)]
             |  dragon(c) -> (green(c) -> happy(c))  [->I(7, 25)]
             |forall x. (dragon(x) -> (green(x) -> happy(x)))  [forallI(6, 26)]""".stripMargin
    )
