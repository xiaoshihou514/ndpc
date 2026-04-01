package ndpc

import cats.effect.unsafe.implicits.global
import ndpc.frontend.checker

class CliBehaviorSpec extends UnitSpec {
    private def expected(rel: String): String =
        os.read(TestPaths.path(rel))

    private def fixtureName(path: os.Path): String =
        path.last.stripSuffix(".ndp")

    private def normalized(text: String): String =
        text.linesIterator.map(_.replaceAll("\\s+$", "")).mkString("\n").trim

    "checker" should "write success output to stderr only" in {
        forAll(os.list(TestPaths.path("test/checker/success/"))) { path =>
            val checkerSuccessInput = path.toString() -> os.read(path)
            val runtime = TestCliRuntime.create(inputs = Map(checkerSuccessInput)).unsafeRunSync()

            checker.check(List(checkerSuccessInput._1), false, runtime).unsafeRunSync() shouldBe 0

            val state = runtime.state.unsafeRunSync()
            state.stdout shouldBe empty
            normalized(state.stderr.text) shouldBe normalized(
              expected("test/stderr/checker-success.txt")
            )
        }
    }

    it should "write human diagnostics to stderr for every checker failure fixture" in {
        forAll(os.list(TestPaths.path("test/checker/failure"))) { input =>
            val runtime =
                TestCliRuntime.create(inputs = Map(input.last -> os.read(input))).unsafeRunSync()

            checker.check(List(input.last), false, runtime).unsafeRunSync() shouldBe 1

            val state = runtime.state.unsafeRunSync()
            state.stdout shouldBe empty
            normalized(state.stderr.text) shouldBe normalized(
              expected(s"test/stderr/checker/${fixtureName(input)}.txt")
            )
        }
    }

    it should "write json diagnostics to stdout for every checker failure fixture" in {
        forAll(os.list(TestPaths.path("test/checker/failure"))) { input =>
            val runtime =
                TestCliRuntime.create(inputs = Map(input.last -> os.read(input))).unsafeRunSync()

            checker.check(List(input.last), true, runtime).unsafeRunSync() shouldBe 1

            val state = runtime.state.unsafeRunSync()
            state.stderr shouldBe empty
            normalized(state.stdout.text) shouldBe normalized(
              expected(s"test/stdout/checker/${fixtureName(input)}.json")
            )
        }
    }
}
