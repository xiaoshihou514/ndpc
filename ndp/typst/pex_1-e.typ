#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($p$, given),
  ($q or not q$, lem),
cases(pf(  ($q$, ass),
  ($p and q$, andi(1, 3)),
  ($p and q or p and not q$, ori(4)),
  ($p and q or p and not q$, tick(5)),
),pf(  ($not q$, ass),
  ($p and not q$, andi(1, 7)),
  ($p and q or p and not q$, ori(8)),
)),  ($p and q or p and not q$, ore(2, 3, 6, 7, 9)),

))