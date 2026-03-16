#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($P -> Q$, premise),
  ($not P -> R$, premise),
  ($Q -> S$, premise),
  ($R -> S$, premise),
  ($P or not P$, lem),
cases(pf(  ($P$, ass),
  ($Q$, impe(1, 6)),
  ($S$, impe(3, 7)),
  ($S$, tick(8)),
),pf(  ($not P$, ass),
  ($R$, impe(2, 10)),
  ($S$, impe(4, 11)),
)),  ($S$, ore(5, 6, 9, 10, 12)),

))