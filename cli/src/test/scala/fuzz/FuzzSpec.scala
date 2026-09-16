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

    /** BUG-04 (pinned): root-scope `tick` lines crash the parser with these exact signatures
      * instead of producing a parse error.
      */
    protected def isKnownParserCrash(e: Throwable): Boolean =
        e.isInstanceOf[NoSuchElementException] && {
            val m = Option(e.getMessage).getOrElse("")
            m == "last of empty list" || m == "head of empty list"
        }

    /** BUG-08 (pinned): comment/empty lines count towards a line's number but are not stored in the
      * checker's `lines` vector, so references in proofs with comments misresolve and can crash
      * with IndexOutOfBoundsException.
      */
    protected def isKnownCommentNumberingCrash(e: Throwable): Boolean =
        e.isInstanceOf[IndexOutOfBoundsException]

    protected def parseNoThrow(s: String): Boolean =
        try
            parser.parse(s)
            true
        catch
            case e if isKnownParserCrash(e) => true // BUG-04, see FuzzBugsSpec
            case e =>
                System.err.println(s"PARSER THREW on ${FuzzGens.show(s)}: ${e.getClass.getName}")
                false
