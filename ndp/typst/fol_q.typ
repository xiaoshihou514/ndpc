#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
  ($forall x. "likes"(x, J o h n)$, premise),
  ($forall y. "likes"(J o h n, y) -> y = J a c k$, premise),
pfbox(
  ($not (J o h n = J a c k)$, ass),
  ($"likes"(J o h n, J o h n)$, fae(1)),
  ($"likes"(J o h n, J o h n) -> J o h n = J a c k$, fae(2)),
  ($J o h n = J a c k$, impe(5, 4)),
  ($bot$, note(3, 6)),
),
  ($J o h n = J a c k$, pc(3, 7)),

))