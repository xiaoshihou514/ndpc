package ndpc

import cats.effect.unsafe.implicits.global
import ndpc.backend.latex

import parsley.Success

class LatexGenSpec extends UnitSpec {
    val inputs = TestPaths.path("ndp/checker/success")
    val outputs = TestPaths.path("ndp/latex")

    "All valid ndp" should "generate valid latex output" in {
        val result = latex.fromSource(os.list(inputs).map(_.toString), ()).unsafeRunSync()
        all(result) shouldBe a[Success[(os.Path, String)]]
        result.map(_.get._1.last) should contain theSameElementsAs os.list(outputs).map(_.last)
    }
}
