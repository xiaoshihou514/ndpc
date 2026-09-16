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

## Bugs found (all fixed)

All of the bugs below were found by this suite. Each one has a minimal
reproduction test in `FuzzBugsSpec.scala` (now active — they encode the fixed
behavior), and the fuzz tolerances for them have been removed, so the
properties are fully strict again.

| tag | area | symptom | fixed in |
|-----|------|---------|----------|
| BUG-01 | `Checker.tryVerify` | single-premise proof rejected, misleading error | 6c8c148 |
| BUG-02 | `Lean.scala:226` | premises-only proof crashed lean codegen (`body.last`) | bc0e17b |
| BUG-03 | `Pretty.parenthesizeString` | `Equiv` under `Implies` printed ambiguously, reparsed to a different tree | bce432e |
| BUG-04 | `Parser` (scope stack) | root-scope `tick` line crashed the parser (`NoSuchElementException`) | d0c9a5a |
| BUG-05 | `Lean.scala:360/471/482/495/547` | identity quantifier/equality substitutions crashed lean codegen | 0462081 |
| BUG-07 | `Formatter.findReasonAlign` | empty proof crashed the formatter (`.max` on empty) | 229a4b5 |
| BUG-08 | `Checker.tryVerifyEach` | comments/empty lines shifted line numbers → out-of-range references crashed | 96abc4c |
| BUG-11 | `Pretty` + `FormulaParser` | `T()` / `F()` parsed as zero-arity predicates but printed as `T`/`F` (Truth/Falsity) | 31bb6a7 |

(No BUG-06: that number was reserved for an `ImpliesElim` argument-order
investigation that turned out to be only a misleading field name in
`Rule.ImpliesElim(ass, imp)` — runtime behavior is consistent.)

Regression tests are all active; `FUZZ_ITER=<n> sbt 'cli/testOnly ndpc.fuzz.*'`
re-runs the property hunt.
