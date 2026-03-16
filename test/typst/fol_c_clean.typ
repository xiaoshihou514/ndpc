#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($exists x. not "P"(x)$, given),
pfbox(
  ($not "P"(c)$, ass),
pfbox(
  ($forall x. "P"(x)$, ass),
  ($"P"(c)$, fae(3)),
  ($bot$, fi(4, 2)),
  ($bot$, tick(5)),
),
  ($not (forall x. "P"(x))$, noti(3, 6)),
  ($not (forall x. "P"(x))$, tick(7)),
),
  ($not (forall x. "P"(x))$, exe(1, 2, 8)),

))