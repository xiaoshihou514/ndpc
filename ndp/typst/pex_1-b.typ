#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
pfbox(
  ($p and q$, ass),
  ($p$, ande(1)),
),
  ($p and q -> p$, impi(1, 2)),

))