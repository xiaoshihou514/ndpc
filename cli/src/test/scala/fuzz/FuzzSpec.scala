package ndpc.fuzz

import org.scalatest.propspec.AnyPropSpec
import org.scalacheck.{Prop, Test}
import ndpc.frontend.parser

/** Shared plumbing for the fuzz specs: run ScalaCheck props natively and turn failures into
  * compact, serializable ScalaTest failures.
  *
  * Bugs already found and pinned in FuzzBugsSpec are tolerated with narrow signatures so the suite
  * stays green while still catching NEW bugs.
  */
abstract class FuzzSpec extends AnyPropSpec:
    private val iters = sys.env.getOrElse("FUZZ_ITER", "300").toInt
    private val params = Test.Parameters.default
        .withMinSuccessfulTests(iters)
        .withMaxDiscardRatio(50f)

    protected def checkProp(name: String, p: Prop): Unit =
        val res = Test.check(params, p)
        if !res.passed then
            val detail = res.status match
                case Test.PropException(args, e, _) =>
                    val trace = e.getStackTrace
                        .take(10)
                        .map("    at " + _.toString)
                        .mkString("\n")
                    s"${e.getClass.getSimpleName}: ${Option(e.getMessage).getOrElse("")}\n" +
                        "  on input: " +
                        args.map(a => FuzzGens.show(String.valueOf(a.arg))).mkString(" ") +
                        s"\n$trace"
                case Test.Failed(args, label) =>
                    "failed on: " +
                        args.map(a => FuzzGens.show(String.valueOf(a.arg))).mkString(" ") +
                        (if label.nonEmpty then s" [$label]" else "")
                case Test.Exhausted => "gave up too often (exhausted)"
                case other          => s"$other"
            fail(s"$name\n$detail".take(4000))

    protected def parseNoThrow(s: String): Boolean =
        try
            parser.parse(s)
            true
        catch
            case e: Throwable =>
                System.err.println(s"PARSER THREW on ${FuzzGens.show(s)}: ${e.getClass.getName}")
                false
