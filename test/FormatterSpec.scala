package ndpc

import ndpc.Formatter.format
import ndpc.Formatter.formattedFromSource

import parsley.Success
import java.io.{PrintStream, ByteArrayOutputStream}

class FormatterSpec extends UnitSpec {
    val inputs = os.FilePath("ndp/formatter/before").resolveFrom(os.pwd)
    val outputs = os.FilePath("ndp/formatter/after").resolveFrom(os.pwd)

    "All valid ndp" should "be formatted according to spec" in {
        val result = formattedFromSource(os.list(inputs).map(_.toString))
        all(result) shouldBe a[Success[(String, String)]]
        result.map(_.get._2) should contain theSameElementsAs os.list(outputs).map(os.read(_))
    }
}
