package ndpc

import ndpc.backend.typst

import parsley.Success

class TypstGenSpec extends UnitSpec {
    val inputs = os.FilePath("ndp/checker/success").resolveFrom(os.pwd)
    val outputs = os.FilePath("ndp/typst").resolveFrom(os.pwd)

    "All valid ndp" should "generate valid typst output" in {
        // val result = typst.fromSource(os.list(inputs).map(_.toString), ())
        // all(result) shouldBe a[Success[(os.Path, String)]]
        // result.map(_.get._2) should contain theSameElementsAs os.list(outputs).map(os.read(_))
    }
}
