#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($D or B$, premise),
  ($not (D or not C)$, premise),
  ($B -> C$, premise),
cases(pf(  ($D$, ass),
  ($D or not C$, ori(4)),
  ($bot$, fi(5, 2)),
  ($C$, fe(6)),
  ($C$, tick(7)),
),pf(  ($B$, ass),
  ($C$, impe(3, 9)),
)),  ($C$, ore(1, 4, 8, 9, 10)),

))