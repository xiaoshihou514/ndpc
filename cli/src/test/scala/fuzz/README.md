# ndpc fuzz suite

Property-based fuzzing for the ndpc core (parser / checker / formatter / codegen
backends), built on ScalaCheck + ScalaTest.

## Run

```bash
sbt 'cli/testOnly ndpc.fuzz.*'                    # default: 300 examples per property
FUZZ_ITER=5000 sbt 'cli/testOnly ndpc.fuzz.*'     # long hunt
FUZZ_ITER=50000 sbt 'cli/testOnly ndpc.fuzz.CodegenFuzzSpec'  # one spec, very long
```

## Layout

- `FuzzGens.scala` — generators (formulas, garbage proofs, mutations of the seed
  files in `test/`), shrinkers, and `knownPrecedenceBug` (BUG-03 shapes).
- `ValidProof.scala` — builds proofs that are valid **by construction**, mirroring
  `Checker`'s semantics. Oracle for "the checker must accept this" and "the
  backends must not crash on this".
- `FuzzSpec.scala` — ScalaCheck plumbing + the narrow `isKnown*` tolerances for
  already-pinned bugs (see `FuzzBugsSpec`). New bug signatures still fail the
  suite, so it is safe to keep green in CI while hunting.
- `ParserFuzzSpec.scala` — parser never throws; printing roundtrips.
- `CheckerFuzzSpec.scala` — checker never throws; formatting is stable
  (reparses, keeps proof lines, idempotent); valid proofs are accepted both as
  AST and as printed text.
- `CodegenFuzzSpec.scala` — latex/typst/lean/html compile valid proofs without
  crashing, deterministically.
- `FuzzBugsSpec.scala` — minimal reproductions for every bug found so far,
  `ignore`d until fixed (remove the `ignore` prefix when fixing).

## Bugs found so far (see FuzzBugsSpec for repros)

| tag | area | symptom |
|-----|------|---------|
| BUG-01 | `Checker.tryVerify` | single-premise proof rejected, misleading error |
| BUG-02 | `Lean.scala:226` | premises-only proof crashes lean codegen (`body.last`) |
| BUG-03 | `Pretty.parenthesizeString` | `Equiv` under `Implies` prints ambiguously, reparses to a different tree |
| BUG-04 | `Parser` (scope stack) | root-scope `tick` line crashes the parser (`NoSuchElementException`) |
| BUG-05 | `Lean.scala:360/471/482/547` | identity quantifier/equality substitutions crash lean codegen |
| BUG-07 | `Formatter.findReasonAlign` | empty proof crashes the formatter (`.max` on empty) |
| BUG-08 | `Checker.tryVerifyEach` | comments/empty lines shift line numbers → valid proofs rejected, `IndexOutOfBoundsException` |
| BUG-11 | `Pretty` + `FormulaParser` | `T()` / `F()` parse as zero-arity predicates but print as `T`/`F` (Truth/Falsity) — formatting changes the proof |

(No BUG-06: that number was reserved for an `ImpliesElim` argument-order
investigation that turned out to be only a misleading field name in
`Rule.ImpliesElim(ass, imp)` — runtime behavior is consistent.)
