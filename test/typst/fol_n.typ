#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($forall x. forall y. "P"(x) -> "Q"(y)$, premise),
pfbox(
  ($a$, fic),
  ($forall y. "P"(a) -> "Q"(y)$, fae(1)),
pfbox(
  ($"P"(a)$, ass),
pfbox(
  ($b$, fic),
  ($"P"(a) -> "Q"(b)$, fae(3)),
  ($"Q"(b)$, impe(6, 4)),
),
  ($forall z. "Q"(z)$, fai(5, 7)),
),
  ($"P"(a) -> (forall z. "Q"(z))$, impi(4, 8)),
),
  ($forall x. "P"(x) -> (forall z. "Q"(z))$, fai(2, 9)),

))