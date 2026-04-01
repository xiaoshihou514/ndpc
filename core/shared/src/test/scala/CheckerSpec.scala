package ndpc

import ndpc.frontend.Checker.checkedFromString
import parsley.{Failure, Success}

class CheckerSpec extends UnitSpec {
    "All valid ndp" should "pass checker validation" in {
        val path = TestPaths.path("test/checker/success")
        val results = os.list(path).map(os.read(_)).map(checkedFromString)
        all(results) shouldBe a[Success[?]]
    }

    "All invalid ndp" should "not pass checker validation" in {
        val path = TestPaths.path("test/checker/failure")
        val results = os.list(path).map(os.read(_)).map(checkedFromString)
        all(results) shouldBe a[Failure[?]]
    }
}
