#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($f -> B or W$, premise),
  ($not (B or P)$, premise),
  ($W -> P$, premise),
pfbox(
  ($B or W$, ass),
cases(pf(  ($B$, ass),
  ($B or P$, ori(5)),
  ($bot$, fi(6, 2)),
  ($bot$, tick(7)),
),pf(  ($W$, ass),
  ($P$, impe(3, 9)),
  ($B or P$, ori(10)),
  ($bot$, fi(11, 2)),
)),  ($bot$, ore(4, 5, 8, 9, 12)),
),
  ($not (B or W)$, noti(4, 13)),
  ($not f$, mt(1, 14)),

))