#import "@preview/boxproof:0.1.0": *
// typst compile *.typ
// Alternatively, preview on https://typst.app
#start(pf(
pfbox(
  ($"R"(c, c)$, ass),
  ($"R"(c, c)$, tick(1)),
),
  ($"R"(c, c) -> "R"(c, c)$, impi(1, 2)),
  ($exists y. "R"(c, y) -> "R"(y, c)$, exi(3)),
  ($exists x. exists y. "R"(x, y) -> "R"(y, x)$, exi(4)),

))