package ndpc

import ndpc.backend.lean

import parsley.Success

class LeanGenSpec extends UnitSpec {
    val inputs = os.FilePath("ndp/checker/success").resolveFrom(os.pwd)
    val outputs = os.FilePath("ndp/lean").resolveFrom(os.pwd)

    "All valid ndp" should "generate valid lean output" in {
        val result = lean.fromSource(os.list(inputs).map(_.toString), ())
        all(result) shouldBe a[Success[(os.Path, String)]]
        result.map(_.get._2) should contain theSameElementsAs os.list(outputs).map(os.read(_))
    }
}
