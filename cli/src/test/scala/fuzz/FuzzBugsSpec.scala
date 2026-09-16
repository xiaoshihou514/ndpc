package ndpc.fuzz

import org.scalatest.*
import flatspec.*
import matchers.*
import ndpc.cli.IORuntime
import ndpc.cli.backend.lean
import ndpc.frontend.{Checker, Formatter}
import ndpc.frontend.expr.formula.*
import ndpc.frontend.parser
import ndpc.frontend.parsers.FormulaParser
import ndpc.frontend.parsers.lexer.{fully, lexeme}
import ndpc.frontend.pretty.*
import cats.effect.unsafe.implicits.global
import parsley.{Failure, Success}

/** Bugs found by the fuzz suite (cli/src/test/scala/fuzz), each pinned with a minimal reproduction.
  *
  * Every test encodes the DESIRED behavior and is `ignore`d until the fix lands: remove the
  * `ignore` prefix to activate. The fuzz properties in ParserFuzzSpec / CheckerFuzzSpec /
  * CodegenFuzzSpec tolerate exactly these signatures (see FuzzSpec.isKnown*), so the suite stays
  * green while still catching new bugs.
  */
class FuzzBugsSpec extends AnyFlatSpec with should.Matchers:
    private def parseFormula(s: String) = fully(lexeme(FormulaParser.lformula)).parse(s)

    "BUG-01 (Checker.tryVerify) a single-premise proof" should "be accepted" in {
        // fixed: tryVerify required `head +: _ :+ tail`, i.e. at least two lines,
        // so the trivially valid one-line proof was rejected with a misleading
        // "did not start with a valid proof" error.
        Checker.checkedFromString("p [premise]\n") match
            case Success(_) => succeed
            case Failure(e) => fail(s"rejected: $e")
    }

    "BUG-02 (Lean.scala:226 body.last) a premises-only proof" should "compile to Lean" in {
        // fixed: pfs.span(Premise|Given) left `body` empty and `.last` threw
        // NoSuchElementException; the result now falls back to the last premise.
        Checker.checkedFromString("p [premise]\nq [given]\n") match
            case Success(pf) =>
                noException should be thrownBy lean.compile(pf, (), IORuntime).unsafeRunSync()
            case Failure(e) => fail(s"rejected: $e")
    }

    "BUG-03 (Pretty.parenthesizeString) Equiv under Implies" should "print unambiguously" in {
        // fixed: the printer treated Equiv as tighter than Implies, but the
        // parser's actual precedence is the reverse (Equiv is loosest), so
        // Implies(X, Equiv(A, B)) printed "X -> A <-> B" which reparses as
        // Equiv(Implies(X, A), B). Equiv is now always parenthesized.
        val f = Implies(
          Exists("c", Eq(PredAp("c71", Nil), PredAp("R", Nil))),
          Equiv(PredAp("Q", Nil), PredAp("P46", Nil))
        )
        parseFormula(f.pretty) match
            case Success(g) => g.shouldBe(f)
            case Failure(e) => fail(s"'${f.pretty}' does not reparse: $e")
    }

    "BUG-04 (Parser State.popScopeWithTick) root-scope tick line" should "error, not crash" in {
        // fixed: a tick line at indent 0 used to pop the root scope, leaving
        // scopeStack empty; the final `scopeStack.last` (or a following line's
        // `scopeStack.head`) threw NoSuchElementException.
        noException should be thrownBy parser.parse("q [tick(1)]\n")
    }

    "BUG-05 (Lean.scala:360/471/482/495/547) identity substitutions" should "compile to Lean" in {
        // The checker accepts identity substitutions (isSubstituteOf allows
        // original == substituted), but the lean backend used to assume the
        // substitution changed something: `ex.diff(...)` returned None and the
        // `val Some(...) = ... : @unchecked` / `.head` / `.get` sites crashed.
        val proofs = List(
          "p [premise]\nexists x. (p) [existsI(1)]\n",
          "forall x. (p) [premise]\np [forallE(1)]\n",
          "exists x. (p) [premise]\n  p [ass]\n  T [TI]\n  T [tick(3)]\nT [existsE(1, 2, 4)]\n",
          "forall x. (p -> q) [premise]\np [premise]\nq [forall->E(2, 1)]\n",
          "a = b [premise]\np(a) [premise]\np(a) [=sub(2, 1)]\n"
        )
        proofs.foreach { text =>
            Checker.checkedFromString(text) match
                case Success(pf) =>
                    val out =
                        try lean.compile(pf, (), IORuntime).unsafeRunSync()
                        catch case e: Throwable => fail(s"lean threw on:\n$text\n$e")
                    out.nonEmpty shouldBe true
                case Failure(e) => fail(s"checker rejected:\n$text\n$e")
        }
    }

    "BUG-07 (Formatter.findReasonAlign) an empty proof" should "format without crashing" in {
        // fixed: `.max` on the empty body list used to throw
        // UnsupportedOperationException("empty.max").
        parser.parse("") match
            case Success(ast) => noException should be thrownBy Formatter.formatPure(ast)
            case Failure(e)   => fail(s"empty input should parse to an empty proof: $e")
    }

    "BUG-08 (Checker.tryVerifyEach) comments must not shift rule line numbers" should "check" in {
        // fixed: lineNr used to accumulate over ALL lines (comments/empties
        // included) while `lines(n - 1)` indexes the proof-lines-only vector, so
        // the accessibility guard used file positions: out-of-range references in
        // proofs with comments crashed with IndexOutOfBoundsException.
        Checker.checkedFromString("p [premise]\n-- c\nq [premise]\np ^ q [^I(1,2)]\n") match
            case Success(_) => succeed
            case Failure(e) => fail(s"valid proof rejected: $e")

        // an out-of-range reference must be a clean failure, not a crash
        Checker.checkedFromString("p [premise]\n-- c\nq [premise]\nq [tick(9)]\n") match
            case Failure(_) => succeed
            case Success(_) => fail("out-of-range tick accepted")
    }

    "BUG-11 (Pretty / FormulaParser) T() and F() as predicate names" should "print unambiguously" in {
        // `F()` parses as PredAp("F", Nil) (empty arg list), but pretty prints it
        // bare as "F", which reparses as Falsity — formatting silently changes
        // the meaning of the proof line.
        parser.parse("F()[existsE(1,0,0)]") match
            case Success(ast) =>
                val formatted = Formatter.formatPure(ast)
                (parser.parse(formatted), FuzzGens.pfLines(ast.main)) match
                    case (Success(re), expected) =>
                        FuzzGens.pfLines(re.main).shouldBe(expected)
                    case (Failure(e), _) => fail(s"formatted '$formatted' does not reparse: $e")
            case Failure(e) => fail(s"'F()[existsE(1,0,0)]' should parse: $e")
    }
