#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($(exists x. "F"(x)) or (exists x. "G"(x))$, given),
cases(pf(  ($exists x. "F"(x)$, ass),
pfbox(
  ($"F"(c)$, ass),
  ($"F"(c) or "G"(c)$, ori(3)),
  ($exists x. "F"(x) or "G"(x)$, exi(4)),
),
  ($exists x. "F"(x) or "G"(x)$, exe(2, 3, 5)),
  ($exists x. "F"(x) or "G"(x)$, tick(6)),
),pf(  ($exists x. "G"(x)$, ass),
pfbox(
  ($"G"(d)$, ass),
  ($"F"(d) or "G"(d)$, ori(9)),
  ($exists x. "F"(x) or "G"(x)$, exi(10)),
),
  ($exists x. "F"(x) or "G"(x)$, exe(8, 9, 11)),
  ($exists x. "F"(x) or "G"(x)$, tick(12)),
)),  ($exists x. "F"(x) or "G"(x)$, ore(1, 2, 7, 8, 13)),

))