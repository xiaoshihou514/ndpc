#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($forall x. not "P"(x)$, given),
pfbox(
  ($exists x. "P"(x)$, ass),
pfbox(
  ($"P"(c)$, ass),
  ($not "P"(c)$, fae(1)),
  ($bot$, note(4, 3)),
  ($bot$, tick(5)),
),
  ($bot$, exe(2, 3, 6)),
  ($bot$, tick(7)),
),
  ($not (exists x. "P"(x))$, noti(2, 8)),

))