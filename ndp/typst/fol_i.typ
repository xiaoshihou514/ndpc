#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($a = b or a = c$, given),
  ($a = b or c = b$, given),
  ($"P"(a) or "P"(b)$, given),
cases(pf(  ($a = b$, ass),
  ($a = b$, tick(4)),
),pf(  ($a = c$, ass),
cases(pf(  ($a = b$, ass),
  ($a = b$, tick(7)),
),pf(  ($c = b$, ass),
  ($a = b$, eqsub(6, 9)),
)),  ($a = b$, ore(2, 7, 8, 9, 10)),
)),  ($a = b$, ore(1, 4, 5, 6, 11)),
cases(pf(  ($"P"(a)$, ass),
  ($"P"(b)$, eqsub(13, 12)),
  ($"P"(a) and "P"(b)$, andi(13, 14)),
  ($"P"(a) and "P"(b)$, tick(15)),
),pf(  ($"P"(b)$, ass),
  ($"P"(a)$, eqsub(17, 12)),
  ($"P"(a) and "P"(b)$, andi(18, 17)),
)),  ($"P"(a) and "P"(b)$, ore(3, 13, 16, 17, 19)),

))