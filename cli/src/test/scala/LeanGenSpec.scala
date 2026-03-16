package ndpc

import cats.effect.unsafe.implicits.global
import ndpc.backend.lean

import parsley.Success

class LeanGenSpec extends UnitSpec {
    val inputs = TestPaths.path("ndp/checker/success")
    val outputs = TestPaths.path("ndp/lean")

    "All valid ndp" should "generate valid lean output" in {
        val result = lean.fromSource(os.list(inputs).map(_.toString), ()).unsafeRunSync()
        all(result) shouldBe a[Success[(os.Path, String)]]
        result.map(_.get._2) should contain theSameElementsAs os.list(outputs).map(os.read(_))
    }
}
