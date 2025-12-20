#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($G or B -> C$, premise),
  ($not D -> not (L -> bot)$, premise),
  ($C -> (L -> bot)$, premise),
pfbox(
  ($G$, ass),
pfbox(
  ($B$, ass),
  ($G or B$, ori(5)),
  ($C$, impe(1, 6)),
  ($L -> bot$, impe(3, 7)),
  ($not (not (L -> bot))$, dni(8)),
  ($not (not D)$, mt(2, 9)),
  ($D$, dne(10)),
),
  ($B -> D$, impi(5, 11)),
),
  ($G -> (B -> D)$, impi(4, 12)),

))