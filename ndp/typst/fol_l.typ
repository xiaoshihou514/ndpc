#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($forall x. "A"(x) and "B"(x) -> "C"(x)$, premise),
  ($exists x. "A"(x) and "B"(x)$, given),
pfbox(
  ($"A"(a) and "B"(a)$, ass),
  ($"A"(a) and "B"(a) -> "C"(a)$, fae(1)),
  ($"C"(a)$, impe(4, 3)),
  ($"A"(a)$, ande(3)),
  ($"A"(a) and "C"(a)$, andi(6, 5)),
  ($exists x. "A"(x) and "C"(x)$, exi(7)),
),
  ($exists x. "A"(x) and "C"(x)$, exe(2, 3, 8)),

))