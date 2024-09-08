package ndpc

import ndpc.Parser.parse
import scala.compiletime.ops.string

class ParserSpec extends UnitSpec {
    "A ndp file" should "be indented, where a line is comment/empty/either/or/just pr" in {
        val input = """-- input 1
        |forall x. (forall y. (child(y, x) -> fly(y)) ^ dragon(x) -> happy(x)) [premise]
        |forall x. (green(x) ^ dragon(x) -> fly(x)) [premise]
        |forall x. (exists y. (parent(y, x) ^ green(y)) -> green(x)) [premise]
        |forall z. forall x. (child (x, z) ^ dragon(z) -> dragon(x)) [premise]
        |forall x. forall y. (child (y, x) -> parent(x, y)) [premise]
        |
        |  c [forall I const]
        |    dragon(c) [ass]
        |      green(c) [ass]
        |        d [forall I const]
        |        child(d, c) [ass]
        |        child (y, x) -> parent(x, y) [forallE(5)]
        |        parent(c, d) [->E(11,10)]
        |        parent(c, d) ^ green(c) [^I(8,11)]
        |        -- <hard struggle>
        |        fly(d) [forall->E(17,2)]
        |        -- some conclusion
        |      T [TI] -- placeholder
        |      -- some more conclusion
        |    T [TI]      -- placeholder
        |    -- and more...
        |  dragon(c) -> (green(c) -> happy(c)) [TI]
        |forall x. (dragon(x) -> (green(x) -> happy(x))) [forallI(6,24)]
        |""".stripMargin
        assert(parse(input).isSuccess)

        val input2 = """-- input 2
            |forall n.(~even(n) -> odd (n)) [premise]
            |forall n.(~odd (n) -> even(n)) [premise]
            |  c [forall I const] -- example comment
            |  ~even(c) -> odd (c) [forallE(1)]
            |  ~odd (c) -> even(c) [forallE(2)]
            |  odd (c) / ~odd (c) [LEM]
            |
            |    -- "box" starts from this line
            |    odd (c)               [ass]
            |    even(c) / odd (c)     [/I(777)]
            |    even(c) / odd (c)     [tick(8)] -- signifiy done
            |    -- "box" ends
            |
            |    ~odd (c)              [ass]
            |    even(c)               [forallE(3)]
            |    even(c) / odd (c)     [/I(10)]
            |    even(c) / odd (c)     [tick(12)]
            |
            |  even(c) / odd (c)       [/E(22,11)]
            |
            |forall n.(even(n) / odd (n)) [forallI(3, 12)]
            |""".stripMargin
        assert(parse(input2).isSuccess)
    }
}
