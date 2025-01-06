package ndpc

import ndpc.Checker.check

import java.io.{PrintStream, ByteArrayOutputStream}

class CheckerSpec extends UnitSpec {
    "All valid ndp" should "pass checker validation" in {
        val path = os.FilePath("ndp/checker/success").resolveFrom(os.pwd)
        System.setErr(PrintStream(ByteArrayOutputStream()))
        check(os.list(path).map(_.toString), false) shouldBe 0
    }

    "All invalid ndp" should "not pass checker validation" in {
        val path = os.FilePath("ndp/checker/failure").resolveFrom(os.pwd)
        val cases = os.list(path).map(_.toString)
        System.setErr(PrintStream(ByteArrayOutputStream()))
        check(cases, false) shouldBe cases.length
    }
}
