#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($p$, given),
pfbox(
  ($q$, ass),
  ($p and q$, andi(1, 2)),
),
  ($q -> p and q$, impi(2, 3)),

))