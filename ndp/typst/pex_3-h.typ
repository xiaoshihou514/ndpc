#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($not t$, premise),
  ($P -> not (R or Q)$, premise),
  ($P -> R or t$, premise),
pfbox(
  ($P$, ass),
  ($R or t$, impe(3, 4)),
  ($not (R or Q)$, impe(2, 4)),
cases(pf(  ($R$, ass),
  ($R or Q$, ori(7)),
  ($bot$, fi(8, 6)),
  ($not Q$, fe(9)),
  ($not Q$, tick(10)),
),pf(  ($t$, ass),
  ($bot$, fi(12, 1)),
  ($not Q$, fe(13)),
)),  ($not Q$, ore(5, 7, 11, 12, 14)),
),
  ($P -> not Q$, impi(4, 15)),

))