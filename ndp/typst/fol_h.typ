#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($(forall x. "F"(x)) and (forall x. "G"(x))$, given),
pfbox(
  ($c$, fic),
  ($forall x. "F"(x)$, ande(1)),
  ($"F"(c)$, fae(3)),
  ($forall x. "G"(x)$, ande(1)),
  ($"G"(c)$, fae(5)),
  ($"F"(c) and "G"(c)$, andi(4, 6)),
),
  ($forall x. "F"(x) and "G"(x)$, fai(2, 7)),

))