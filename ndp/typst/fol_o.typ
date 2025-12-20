#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($forall x. "P"(x) or "Q"(x)$, premise),
  ($not (forall x. "P"(x))$, premise),
pfbox(
  ($forall x. not "Q"(x)$, ass),
pfbox(
  ($c$, fic),
  ($"P"(c) or "Q"(c)$, fae(1)),
cases(pf(  ($"P"(c)$, ass),
  ($"P"(c)$, tick(6)),
),pf(  ($"Q"(c)$, ass),
  ($not "Q"(c)$, fae(3)),
  ($bot$, note(9, 8)),
  ($"P"(c)$, fe(10)),
)),  ($"P"(c)$, ore(5, 6, 7, 8, 11)),
),
  ($forall x. "P"(x)$, fai(4, 12)),
  ($bot$, note(2, 13)),
),
  ($not (forall x. not "Q"(x))$, noti(3, 14)),

))