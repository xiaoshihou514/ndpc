#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($R -> not I$, premise),
  ($I or f$, premise),
  ($not f$, premise),
cases(pf(  ($I$, ass),
  ($not (not I)$, dni(4)),
  ($not R$, mt(1, 5)),
  ($not R$, tick(6)),
),pf(  ($f$, ass),
  ($bot$, fi(8, 3)),
  ($not R$, fe(9)),
)),  ($not R$, ore(2, 4, 7, 8, 10)),

))