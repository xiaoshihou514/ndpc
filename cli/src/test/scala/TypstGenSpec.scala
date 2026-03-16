package ndpc

import cats.effect.unsafe.implicits.global
import ndpc.backend.typst

import parsley.Success

class TypstGenSpec extends UnitSpec {
    val inputs = TestPaths.path("test/checker/success")
    val outputs = TestPaths.path("test/typst")

    "All valid ndp" should "generate valid typst output" in {
        val result = typst.fromSource(os.list(inputs).map(_.toString), ()).unsafeRunSync()
        all(result) shouldBe a[Success[(os.Path, String)]]
        result.map(_.get._1.last) should contain theSameElementsAs os.list(outputs).map(_.last)
    }
}
