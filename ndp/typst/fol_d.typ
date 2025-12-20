#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($not (forall x. "P"(x))$, given),
pfbox(
  ($not (exists x. not "P"(x))$, ass),
pfbox(
  ($c$, fic),
pfbox(
  ($not "P"(c)$, ass),
  ($exists x. not "P"(x)$, exi(4)),
  ($bot$, note(2, 5)),
),
  ($"P"(c)$, pc(4, 6)),
),
  ($forall x. "P"(x)$, fai(3, 7)),
  ($bot$, note(1, 8)),
),
  ($exists x. not "P"(x)$, pc(2, 9)),

))