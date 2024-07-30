package ndpc

import ndpc.Parser.parse

class ParserSpec extends UnitSpec {
    "A ndp file" should "be indented, a line is comment/empty/either/or/just pr" in {
        println(
          parse(
            """
forall x. (forall y. (child(y, x) -> fly(y)) ^ dragon(x) -> happy(x)) [premise]
forall x. (green(x) ^ dragon(x) -> fly(x)) [premise]
forall x. (exists y. (parent(y, x) ^ green(y)) -> green(x)) [premise]
forall z x. (child (x, z) ^ dragon(z) -> dragon(x)) [premise]
forall x y. (child (y, x) -> parent(x, y)) [premise]

  c [forall I const]
    dragon(c) [ass]
      green(c) [ass]
        d [forall I const]
        child(d, c) [ass]
        child (y, x) -> parent(x, y) [forallE(5)]
        parent(c, d) [->E(11,10)]
        parent(c, d) ^ green(c) [^I(8,11)]
        -- <hard struggle>
        fly(d) [forall->E(17,2)]
        -- some conclusion
      -- some more conclusion
    -- and more...
  dragon(c) -> (green(c) -> happy(c)) [TI]
forall x. (dragon(x) -> (green(x) -> happy(x))) [forallI(6,24)]
""".trim() + "\n"
          )
        )
    }
}
