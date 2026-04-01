#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($C and N -> t$, given),
  ($H and not S$, given),
  ($H and not (S or C) -> P$, given),
pfbox(
  ($N and not t$, ass),
  ($N$, ande(4)),
  ($not t$, ande(4)),
pfbox(
  ($C$, ass),
  ($C and N$, andi(7, 5)),
  ($t$, impe(1, 8)),
  ($bot$, fi(9, 6)),
),
  ($not C$, noti(7, 10)),
  ($not S$, ande(2)),
pfbox(
  ($S or C$, ass),
cases(pf(  ($S$, ass),
  ($bot$, fi(14, 12)),
  ($bot$, tick(15)),
),pf(  ($C$, ass),
  ($bot$, fi(17, 11)),
)),  ($bot$, ore(13, 14, 16, 17, 18)),
),
  ($not (S or C)$, noti(13, 19)),
  ($H$, ande(2)),
  ($H and not (S or C)$, andi(21, 20)),
  ($P$, impe(3, 22)),
),
  ($N and not t -> P$, impi(4, 23)),

))