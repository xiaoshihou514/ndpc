#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($forall n. not "even"(n) -> "odd"(n)$, premise),
  ($forall n. not "odd"(n) -> "even"(n)$, premise),
pfbox(
  ($c$, fic),
  ($not "even"(c) -> "odd"(c)$, fae(1)),
  ($not "odd"(c) -> "even"(c)$, fae(2)),
  ($"odd"(c) or not "odd"(c)$, lem),
cases(pf(  ($"odd"(c)$, ass),
  ($"even"(c) or "odd"(c)$, ori(7)),
  ($"even"(c) or "odd"(c)$, tick(8)),
),pf(  ($not "odd"(c)$, ass),
  ($"even"(c)$, impe(5, 10)),
  ($"even"(c) or "odd"(c)$, ori(11)),
)),  ($"even"(c) or "odd"(c)$, ore(6, 7, 9, 10, 12)),
),
  ($forall n. "even"(n) or "odd"(n)$, fai(3, 13)),

))