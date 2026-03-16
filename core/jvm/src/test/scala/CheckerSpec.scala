package ndpc

import ndpc.frontend.checkerCore.checkedFromString
import parsley.{Failure, Success}

class CheckerSpec extends UnitSpec {
    "All valid ndp" should "pass checker validation" in {
        val path = TestPaths.path("ndp/checker/success")
        val results = os.list(path).map(os.read(_)).map(checkedFromString)
        all(results) shouldBe a[Success[?]]
    }

    "All invalid ndp" should "not pass checker validation" in {
        val path = TestPaths.path("ndp/checker/failure")
        val results = os.list(path).map(os.read(_)).map(checkedFromString)
        all(results) shouldBe a[Failure[?]]
    }
}
