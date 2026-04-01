#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($not P$, premise),
  ($B or W -> P$, premise),
  ($not I -> B$, premise),
  ($not W -> M$, premise),
  ($L -> not I and not M$, premise),
pfbox(
  ($L$, ass),
  ($not I and not M$, impe(5, 6)),
  ($not I$, ande(7)),
  ($B$, impe(3, 8)),
  ($B or W$, ori(9)),
  ($P$, impe(2, 10)),
  ($bot$, fi(11, 1)),
),
  ($not L$, noti(6, 12)),

))