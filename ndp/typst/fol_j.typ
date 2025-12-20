#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($(forall x. x = a) or x = b$, premise),
  ($"g"(a) = b$, premise),
  ($forall x. forall y. "g"(x) = "g"(y) -> x = y$, premise),
  ($"g"(b) = a or "g"(b) = b$, premise),
cases(pf(  ($"g"(b) = a$, ass),
  ($"g"("g"(a)) = a$, eqsub(5, 2)),
  ($"g"("g"(a)) = a$, tick(6)),
),pf(  ($"g"(b) = b$, ass),
  ($"g"(b) = "g"(a)$, eqsub(8, 2)),
  ($forall y. "g"(b) = "g"(y) -> b = y$, fae(3)),
  ($b = a$, faie(9, 10)),
  ($"g"(a) = a$, eqsub(8, 11)),
  ($"g"("g"(a)) = a$, eqsub(12, 12)),
)),  ($"g"("g"(a)) = a$, ore(4, 5, 7, 8, 13)),

))