package ndpc

import parsley.Parsley
import parsley.character.satisfy

class ParserSpec extends UnitSpec {
    "A constant" should "be any non-space value" in {
        val const = satisfy(_ != ' ')
        assert(const.parse("a").get === 'a')
    }
}
