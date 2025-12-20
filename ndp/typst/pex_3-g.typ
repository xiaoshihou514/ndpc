#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($R -> (B -> D or L)$, given),
  ($not (D or G)$, given),
  ($L or B -> G$, given),
pfbox(
  ($B$, ass),
  ($L or B$, ori(4)),
  ($G$, impe(3, 5)),
  ($D or G$, ori(6)),
  ($bot$, fi(7, 2)),
  ($not R$, fe(8)),
),
  ($B -> not R$, impi(4, 9)),

))