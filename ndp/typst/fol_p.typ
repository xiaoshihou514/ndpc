#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($a = b$, premise),
  ($not (b = b and b = c)$, premise),
pfbox(
  ($a = c$, ass),
  ($c = b$, eqsub(1, 3)),
  ($b = c$, symm(4)),
  ($b = b$, refl),
  ($b = b and b = c$, andi(6, 5)),
  ($bot$, note(2, 7)),
),
  ($not (a = c)$, noti(3, 8)),

))