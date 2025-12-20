#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($forall n. not "even"(n) -> "odd"(n)$, premise),
  ($forall n. "odd"(n) -> not "even"(n)$, premise),
pfbox(
  ($a$, fic),
  ($"odd"(a) -> not "even"(a)$, fae(2)),
pfbox(
  ($"even"(a) and "odd"(a)$, ass),
  ($"odd"(a)$, ande(5)),
  ($not "even"(a)$, impe(4, 6)),
  ($"even"(a)$, ande(5)),
  ($bot$, note(7, 8)),
),
  ($not ("even"(a) and "odd"(a))$, noti(5, 9)),
),
  ($forall n. not ("even"(n) and "odd"(n))$, fai(3, 10)),

))