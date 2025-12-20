#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($forall x. "A"(x) or "B"(x)$, premise),
  ($forall y. not "A"(y)$, premise),
pfbox(
  ($c$, fic),
  ($"A"(c) or "B"(c)$, fae(1)),
  ($not "A"(c)$, fae(2)),
cases(pf(  ($"A"(c)$, ass),
  ($bot$, note(5, 6)),
  ($"B"(c)$, fe(7)),
  ($"B"(c)$, tick(8)),
),pf(  ($"B"(c)$, ass),
  ($"B"(c)$, tick(10)),
)),  ($"B"(c)$, ore(4, 6, 9, 10, 11)),
),
  ($forall x. "B"(x)$, fai(3, 12)),

))