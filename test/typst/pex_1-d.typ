#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($p -> (q -> r)$, given),
pfbox(
  ($p -> q$, ass),
pfbox(
  ($p$, ass),
  ($q$, impe(2, 3)),
  ($q -> r$, impe(1, 3)),
  ($r$, impe(5, 4)),
),
  ($p -> r$, impi(3, 6)),
),
  ($(p -> q) -> (p -> r)$, impi(2, 7)),

))