#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
pfbox(
  ($p$, ass),
pfbox(
  ($q$, ass),
  ($p$, tick(1)),
),
  ($q -> p$, impi(2, 3)),
),
  ($p -> (q -> p)$, impi(1, 4)),

))