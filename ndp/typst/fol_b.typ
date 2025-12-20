#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($not (exists x. "P"(x))$, given),
pfbox(
  ($c$, fic),
pfbox(
  ($"P"(c)$, ass),
  ($exists x. "P"(x)$, exi(3)),
  ($bot$, note(1, 4)),
),
  ($not "P"(c)$, noti(3, 5)),
),
  ($forall x. not "P"(x)$, fai(2, 6)),

))