package ndpc.fuzz

import org.scalacheck.Prop
import ndpc.cli.IORuntime
import ndpc.cli.backend.{html, latex, lean, typst}
import ndpc.frontend.CheckedProof
import ndpc.frontend.Checker
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import parsley.{Failure, Success}

/** Codegen fuzzing: every backend must compile valid proofs without crashing, deterministically.
  */
class CodegenFuzzSpec extends FuzzSpec:

    private def runBackend(name: String, io: IO[String]): Boolean =
        try
            val first = io.unsafeRunSync()
            val second = io.unsafeRunSync()
            if first == second then true
            else
                System.err.println(s"$name backend is not deterministic")
                false
        catch
            // BUG-02 (pinned): lean crashes on premises-only proofs (`body.last`)
            case e: NoSuchElementException
                if name == "lean" && Option(e.getMessage).exists(_.contains("last of empty")) =>
                true
            // BUG-05 (pinned): lean MatchError on identity quantifier substitutions
            // (Lean.scala `val Some(PredAp(name, Nil)) = ex.diff(...): @unchecked`)
            case e: MatchError
                if name == "lean" && Option(e.getMessage)
                    .exists(_.startsWith("None (of class scala.None$)")) =>
                true
            case e =>
                System.err.println(
                  s"$name backend threw ${e.getClass.getName}: ${e.getMessage}"
                )
                false

    property(
      "BUG-HUNT codegen: backends compile valid proofs without crashing, deterministically"
    ) {
        checkProp(
          "codegen",
          Prop.forAll(ValidProof.gen) { vp =>
              try
                  Checker.checkParsed(vp.ast) match
                      case Success(pf: CheckedProof) =>
                          runBackend("latex", latex.compile(pf, (), IORuntime)) &&
                          runBackend("typst", typst.compile(pf, (), IORuntime)) &&
                          runBackend("lean", lean.compile(pf, (), IORuntime)) &&
                          runBackend("html", html.compile(pf, None, IORuntime))
                      case Success(_) => false
                      case Failure(err) =>
                          System.err.println(
                            s"checker rejected valid proof:\n${vp.text}\nerror: $err"
                          )
                          false
              catch
                  // BUG-02/05 (pinned family): the lean backend crashes on several
                  // checker-valid proofs — premises-only (`body.last`, Lean.scala:226)
                  // and identity quantifier/equality substitutions
                  // (Lean.scala:360/471/482/547). Tolerate any NSEE/MatchError raised
                  // inside the lean backend, nothing else.
                  case e @ (_: NoSuchElementException | _: MatchError)
                      if e.getStackTrace
                          .exists(_.getClassName.startsWith("ndpc.cli.backend.lean")) =>
                      System.err.println(
                        s"BUG-02/05 signature: ${e.getClass.getSimpleName}: ${e.getMessage}"
                      )
                      true
                  case e: Throwable =>
                      System.err.println(
                        s"CODEGEN PROPERTY THREW ${e.getClass.getName}: ${e.getMessage}\n" +
                            e.getStackTrace.take(8).map("  at " + _).mkString("\n") +
                            s"\nproof:\n${vp.text}"
                      )
                      false
          }
        )
    }
