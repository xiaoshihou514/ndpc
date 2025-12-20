#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($p and q$, premise),
  ($r$, premise),
  ($q$, ande(1)),
  ($q and r$, andi(3, 2)),

))