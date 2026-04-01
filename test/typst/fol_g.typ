#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($forall x. "F"(x) and "G"(x)$, given),
pfbox(
  ($c 1$, fic),
  ($"F"(c 1) and "G"(c 1)$, fae(1)),
  ($"F"(c 1)$, ande(3)),
),
  ($forall x. "F"(x)$, fai(2, 4)),
pfbox(
  ($c 2$, fic),
  ($"F"(c 2) and "G"(c 2)$, fae(1)),
  ($"G"(c 2)$, ande(7)),
),
  ($forall x. "G"(x)$, fai(6, 8)),
  ($(forall x. "F"(x)) and (forall x. "G"(x))$, andi(5, 9)),

))