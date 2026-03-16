package ndpc

import ndpc.frontend.formatterCore.formattedFromString
import parsley.Success

class FormatterSpec extends UnitSpec {
    val inputs = TestPaths.path("test/formatter/before")
    val outputs = TestPaths.path("test/formatter/after")

    "All valid ndp" should "be formatted according to spec" in {
        val result = os.list(inputs).map(os.read(_)).map(formattedFromString)
        all(result) shouldBe a[Success[String]]
        result.map(_.get) should contain theSameElementsAs os.list(outputs).map(os.read(_))
    }
}
