#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($exists x. "F"(x) or "G"(x)$, given),
pfbox(
  ($"F"(c) or "G"(c)$, ass),
cases(pf(  ($"F"(c)$, ass),
  ($exists x. "F"(x)$, exi(3)),
  ($(exists x. "F"(x)) or (exists x. "G"(x))$, ori(4)),
  ($(exists x. "F"(x)) or (exists x. "G"(x))$, tick(5)),
),pf(  ($"G"(c)$, ass),
  ($exists x. "G"(x)$, exi(7)),
  ($(exists x. "F"(x)) or (exists x. "G"(x))$, ori(8)),
)),  ($(exists x. "F"(x)) or (exists x. "G"(x))$, ore(2, 3, 6, 7, 9)),
),
  ($(exists x. "F"(x)) or (exists x. "G"(x))$, exe(1, 2, 10)),

))