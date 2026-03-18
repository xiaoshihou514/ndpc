package ndpc.web

object Docs:
    /** Maps every rule token string to a human-readable description. */
    val docs: Map[String, String] = Map(
        // ---------- premises / assumptions ----------
        "premise"        -> "Premise: marks a given proposition at the start of a proof.",
        "given"          -> "Given: same as premise; marks a starting given proposition.",
        "ass"            -> "Assumption (ass): opens a sub-proof box by assuming a formula. Must be closed with tick.",
        "tick"           -> "Tick (✓): closes the current sub-proof box after its conclusion is established. Usage: [tick(n)]",
        // ---------- introduction rules ----------
        "^I"             -> "∧-introduction (^I): from A on line m and B on line n, derive A ∧ B. Usage: [^I(m, n)]",
        "->I"            -> "→-introduction (->I): from assumption at line m to conclusion at line n, derive A → B. Usage: [->I(m, n)]",
        "/I"             -> "∨-introduction (/I): from A on line m, derive A ∨ B or B ∨ A. Usage: [/I(m)]",
        "~I"             -> "¬-introduction (~I): from assumption at line m leading to ⊥ at line n, derive ¬A. Usage: [~I(m, n)]",
        "~~I"            -> "¬¬-introduction (~~I): from A on line m, derive ¬¬A. Usage: [~~I(m)]",
        "FI"             -> "⊥-introduction (FI): from A on line m and ¬A on line n, derive ⊥. Usage: [FI(m, n)]",
        "TI"             -> "⊤-introduction (TI): derive ⊤ anywhere; no premises needed. Usage: [TI]",
        "<->I"           -> "↔-introduction (<->I): from (A → B) on line m and (B → A) on line n, derive A ↔ B. Usage: [<->I(m, n)]",
        "existsI"        -> "∃-introduction (existsI): from A[t/x] on line m, derive ∃x A. Usage: [existsI(m)]",
        "forallI"        -> "∀-introduction (forallI): from A[c/x] proved at line m using fresh constant at line n, derive ∀x A. Usage: [forallI(n, m)]",
        // ---------- elimination rules ----------
        "^E"             -> "∧-elimination (^E): from A ∧ B on line m, derive A or B. Usage: [^E(m)]",
        "->E"            -> "→-elimination (->E, modus ponens): from A on line m and A → B on line n, derive B. Usage: [->E(m, n)]",
        "/E"             -> "∨-elimination (/E): from A ∨ B on line m, sub-proofs ending at lines (l1,l2) and (l3,l4), derive C. Usage: [/E(m, l1, l2, l3, l4)]",
        "~E"             -> "¬-elimination (~E): from A on line m and ¬A on line n, derive ⊥. Usage: [~E(m, n)]",
        "~~E"            -> "¬¬-elimination (~~E): from ¬¬A on line m, derive A. Usage: [~~E(m)]",
        "FE"             -> "⊥-elimination (FE): from ⊥ on line m, derive any formula. Usage: [FE(m)]",
        "<->E"           -> "↔-elimination (<->E): from A ↔ B on line m and A (or B) on line n, derive B (or A). Usage: [<->E(m, n)]",
        "existsE"        -> "∃-elimination (existsE): from ∃x A on line m, assumption at l1, conclusion at l2, derive C. Usage: [existsE(m, l1, l2)]",
        "forallE"        -> "∀-elimination (forallE): from ∀x A on line m, derive A[t/x] for any term t. Usage: [forallE(m)]",
        "forall->E"      -> "∀→-elimination: from ∀x(A → B) and A[t/x], derive B[t/x] in one step. Usage: [forall->E(m, n)]",
        // ---------- classical / derived rules ----------
        "LEM"            -> "Law of Excluded Middle (LEM): derive A ∨ ¬A for any formula A. Usage: [LEM]",
        "MT"             -> "Modus Tollens (MT): from A → B on line m and ¬B on line n, derive ¬A. Usage: [MT(m, n)]",
        "PC"             -> "Proof by Contradiction (PC): from assumption ¬A at line m leading to ⊥ at line n, derive A. Usage: [PC(m, n)]",
        "refl"           -> "Reflexivity (refl): derive a = a for any term a. No arguments needed. Usage: [refl]",
        "=sub"           -> "Equality substitution (=sub): from A[a/x] on line m and a = b on line n, derive A[b/x]. Usage: [=sub(m, n)]",
        "sym"            -> "Symmetry (sym): from a = b on line m, derive b = a. Usage: [sym(m)]",
        "forall I const" -> "Forall introduction constant: introduce a fresh constant for use with forallI. Usage: [forall I const]",
        // ---------- logical keywords (outside brackets) ----------
        "forall"         -> "Universal quantifier: forall x. φ — the formula φ holds for all x.",
        "exists"         -> "Existential quantifier: exists x. φ — the formula φ holds for some x.",
    )
