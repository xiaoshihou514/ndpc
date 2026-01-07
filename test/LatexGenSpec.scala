package ndpc

import ndpc.backend.latex

import parsley.Success

class LatexGenSpec extends UnitSpec {
    val inputs = os.FilePath("ndp/checker/success").resolveFrom(os.pwd)
    val outputs = os.FilePath("ndp/latex").resolveFrom(os.pwd)

    "All valid ndp" should "generate valid latex output" in {
        // val result = latex.fromSource(os.list(inputs).map(_.toString), ())
        // all(result) shouldBe a[Success[(os.Path, String)]]
        // result.map(_.get._2) should contain theSameElementsAs os.list(outputs).map(os.read(_))
    }
}
