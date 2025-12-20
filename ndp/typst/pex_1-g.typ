#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($p and q -> r$, given),
pfbox(
  ($p$, ass),
pfbox(
  ($q$, ass),
  ($p and q$, andi(2, 3)),
  ($r$, impe(1, 4)),
),
  ($q -> r$, impi(3, 5)),
),
  ($p -> (q -> r)$, impi(2, 6)),

))