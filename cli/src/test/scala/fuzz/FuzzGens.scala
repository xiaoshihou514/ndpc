package ndpc.fuzz

import org.scalacheck.{Gen, Shrink}
import ndpc.frontend.expr.formula.*
import ndpc.frontend.expr.rule.*
import ndpc.frontend.parser
import ndpc.frontend.parsers.FormulaParser
import ndpc.frontend.parsers.lexer.{fully, lexeme}
import parsley.{Failure, Success}

import scala.util.Try

/** Generators and shrinkers for fuzzing the ndpc core (parser / checker / codegen). */
object FuzzGens:

    // ── names ────────────────────────────────────────────────────────────
    // identifiers in the lexer are anything not in "( ) [ ] < > space . , ~ = ^ / -"
    // and "forall"/"exists" are hard keywords. Bare "T"/"F" parse as truth/falsity,
    // so roundtrip-safe names avoid those.
    val safeName: Gen[String] =
        for
            base <- Gen.oneOf("p", "q", "r", "s", "x", "y", "z", "P", "Q", "R", "c")
            suffix <- Gen.frequency(
              (3, Gen.const("")),
              (1, Gen.choose(1, 99).map(_.toString))
            )
        yield base + suffix

    val weirdChars: String = "abcdefgABCDEFG0123456789_'\"\\{}$%&*+!?;:@|#`. §äöüλ→∀∃"
    val weirdName: Gen[String] =
        Gen.nonEmptyListOf(Gen.oneOf(weirdChars.toList)).map(_.mkString.trim).filter(_.nonEmpty)

    // ── formulas ─────────────────────────────────────────────────────────
    // terms: parser-reachable predicate applications
    def genTerm(depth: Int, names: Gen[String]): Gen[LFormula] =
        if depth <= 0 then names.map(PredAp(_, Nil))
        else
            Gen.frequency(
              (6, names.map(PredAp(_, Nil))),
              (
                1,
                for
                    p <- names
                    k <- Gen.choose(1, 3)
                    args <- Gen.listOfN(k, genTerm(depth - 1, names))
                yield PredAp(p, args)
              )
            )

    def genAtom(depth: Int, names: Gen[String]): Gen[LFormula] =
        Gen.frequency(
          (4, names.map(PredAp(_, Nil))),
          (
            2,
            for
                p <- names
                k <- Gen.choose(1, 2)
                args <- Gen.listOfN(k, genTerm(math.max(0, depth - 1), names))
            yield PredAp(p, args)
          ),
          (
            2,
            for
                a <- genTerm(depth, names)
                b <- genTerm(depth, names)
            yield Eq(a, b)
          ),
          (1, Gen.const(Truth)),
          (1, Gen.const(Falsity))
        )

    def genFormula(depth: Int, names: Gen[String] = safeName): Gen[LFormula] =
        if depth <= 0 then genAtom(0, names)
        else
            Gen.frequency(
              (4, genAtom(depth, names)),
              (1, genFormula(depth - 1, names).map(Not.apply)),
              (
                1,
                for l <- genFormula(depth - 1, names); r <- genFormula(depth - 1, names)
                yield And(l, r)
              ),
              (
                1,
                for l <- genFormula(depth - 1, names); r <- genFormula(depth - 1, names)
                yield Or(l, r)
              ),
              (
                1,
                for l <- genFormula(depth - 1, names); r <- genFormula(depth - 1, names)
                yield Implies(l, r)
              ),
              (
                1,
                for l <- genFormula(depth - 1, names); r <- genFormula(depth - 1, names)
                yield Equiv(l, r)
              ),
              (1, for x <- names; b <- genFormula(depth - 1, names) yield Forall(x, b)),
              (1, for x <- names; b <- genFormula(depth - 1, names) yield Exists(x, b))
            )

    val genLFormula: Gen[LFormula] =
        Gen.sized(n => genFormula(math.max(0, math.min(n, 3)), safeName))
    val genWeirdFormula: Gen[LFormula] =
        Gen.sized(n => genFormula(math.max(0, math.min(n, 2)), weirdName))

    def children(f: LFormula): List[LFormula] = f match
        case PredAp(_, args) => args
        case Eq(l, r)        => List(l, r)
        case Not(pf)         => List(pf)
        case And(l, r)       => List(l, r)
        case Or(l, r)        => List(l, r)
        case Implies(l, r)   => List(l, r)
        case Equiv(l, r)     => List(l, r)
        case Forall(_, b)    => List(b)
        case Exists(_, b)    => List(b)
        case Truth | Falsity => Nil

    // ── BUG-03 shape analysis (empirical) ────────────────────────────────
    // parsley's precedence combinator treats the FIRST listed operator as the
    // tightest, so the parser's real precedence (loosest first) is:
    //   Equiv < Implies < Or < And < forall/exists/Not < Eq
    // The pretty printer's parenthesizeString instead assumes Implies is looser
    // than Equiv (prec Implies=1 < Equiv=2), so an Equiv that is a direct child
    // of an Implies is printed bare and reparses with a different tree:
    //   pretty(Implies(X, Equiv(A, B)))  =  "X -> A <-> B"
    //   reparses as                      =  Equiv(Implies(X, A), B)
    def knownPrecedenceBug(f: LFormula): Boolean =
        val edgeBad = f match
            case Implies(l, r) => l.isInstanceOf[Equiv] || r.isInstanceOf[Equiv]
            case _             => false
        edgeBad || children(f).exists(knownPrecedenceBug)

    /** BUG-11 (pinned): the parser accepts `T()` / `F()` as a zero-arity PredAp named T or F, but
      * pretty prints it bare, where it re-parses as Truth / Falsity — formatting silently changes
      * the meaning of the proof line.
      */
    /** Formulas whose pretty-printing does not survive a reparse. */
    def knownPrintRoundtripBug(f: LFormula): Boolean = knownPrecedenceBug(f)

    /** Formulas guaranteed to survive a print/parse roundtrip. */
    val genRoundtripFormula: Gen[LFormula] =
        FuzzGens.genFormula(2, safeName).retryUntil(f => !knownPrecedenceBug(f))

    // ── standalone formula parser (for roundtrip properties) ─────────────
    def parseFormula(s: String) = fully(lexeme(FormulaParser.lformula)).parse(s)

    // ── shrinks ──────────────────────────────────────────────────────────
    given shrinkFormula: Shrink[LFormula] = Shrink {
        case And(l, r)         => Stream(l, r)
        case Or(l, r)          => Stream(l, r)
        case Implies(l, r)     => Stream(l, r)
        case Equiv(l, r)       => Stream(l, r)
        case Eq(l, r)          => Stream(l, r)
        case Not(pf)           => Stream(pf)
        case Forall(_, b)      => Stream(b)
        case Exists(_, b)      => Stream(b)
        case PredAp(_, a :: _) => Stream(a)
        case _                 => Stream.empty
    }

    // ── raw string soup ──────────────────────────────────────────────────
    val soupAlphabet: List[Char] =
        "pqrsxyz()[]~^/-><=&|., \t\n0123456789IiEeFfLMPCTalfaexsivbdrn\"\\{}".toList

    val genSoup: Gen[String] =
        for
            n <- Gen.choose(1, 160)
            cs <- Gen.listOfN(n, Gen.oneOf(soupAlphabet))
        yield cs.mkString

    // ── structure-aware garbage proofs ───────────────────────────────────
    val ruleNames: List[(String, Int)] = List(
      ("^I", 2),
      ("^E", 1),
      ("->I", 2),
      ("->E", 2),
      ("/I", 1),
      ("/E", 5),
      ("~I", 2),
      ("~E", 2),
      ("~~I", 1),
      ("~~E", 1),
      ("FI", 2),
      ("FE", 1),
      ("<->I", 2),
      ("<->E", 2),
      ("TI", 0),
      ("LEM", 0),
      ("MT", 2),
      ("PC", 2),
      ("refl", 0),
      ("=sub", 2),
      ("sym", 1),
      ("forallI", 2),
      ("forallE", 1),
      ("forall->E", 2),
      ("existsI", 1),
      ("existsE", 3),
      ("tick", 1),
      ("ass", 0),
      ("premise", 0),
      ("given", 0),
      ("forall I const", 0)
    )

    def genRuleStr(maxRef: Int): Gen[String] =
        for
            (name, arity) <- Gen.oneOf(ruleNames)
            refs <- Gen.listOfN(arity, Gen.choose(1, math.max(1, maxRef) + 2))
        yield if arity == 0 then name else s"$name(${refs.mkString(",")})"

    type Out = (List[String], Int)

    private def genPfLineThen(indent: Int, budget: Int, maxRef: Int): Gen[Out] =
        for
            f <- genFormula(2, safeName)
            ruleStr <- genRuleStr(maxRef)
            first = s"${"  " * indent}$f [$ruleStr]"
            (rest, m) <- genBodyLines(indent, budget - 1, maxRef + 1)
        yield (first :: rest, m)

    private def genBoxThen(indent: Int, budget: Int, maxRef: Int): Gen[Out] =
        for
            (inner, m0) <- genBodyLines(indent + 1, budget - 1, maxRef + 1)
            (rest, m) <- genBodyLines(indent, budget - 1, m0)
        yield (inner ::: rest, m)

    private def genCommentThen(indent: Int, budget: Int, maxRef: Int): Gen[Out] =
        for
            c <- Gen.oneOf("hello", "", "p [premise]", "-- x", "[", "]")
            first = s"${"  " * indent}-- $c"
            (rest, m) <- genBodyLines(indent, budget - 1, maxRef)
        yield (first :: rest, m)

    private def genBodyLines(indent: Int, budget: Int, maxRef: Int): Gen[Out] =
        if budget <= 0 then Gen.const((Nil, maxRef))
        else
            Gen.frequency(
              (6, genPfLineThen(indent, budget, maxRef)),
              (1, genCommentThen(indent, budget, maxRef)),
              (2, genBoxThen(indent, budget, maxRef)),
              (1, Gen.const((Nil, maxRef)))
            )

    val genGarbageProof: Gen[String] =
        Gen.sized { n =>
            val budget = math.max(1, math.min(n, 8))
            for
                nPre <- Gen.choose(1, 3)
                prems <- Gen.listOfN(nPre, genFormula(2, safeName))
                premLines = prems.map(f => s"$f [premise]")
                (body, _) <- genBodyLines(0, budget, nPre)
            yield (premLines ::: body).mkString("\n") + "\n"
        }

    // ── seed corpus + mutations ──────────────────────────────────────────
    lazy val seeds: List[String] =
        val root = Try(os.Path(sys.props.getOrElse("ndpc.repoRoot", os.pwd.toString), os.pwd))
            .getOrElse(os.pwd)
        val files = Try(
          os.list(root / "test" / "checker")
              .flatMap(os.list(_))
              .filter(_.ext == "ndp")
              .take(24)
              .toList
              .map(os.read(_).take(1200))
        ).getOrElse(Nil)
        val main = Try(os.read(root / "test.ndp")).getOrElse("")
        (main :: files).filter(_.nonEmpty)

    val mutAlphabet: List[Char] = soupAlphabet

    def changeNumber(s: String): String =
        val rx = "[0-9]+".r
        val ms = rx.findAllIn(s).toList
        if ms.isEmpty then s
        else
            val target = ms(scala.util.Random.nextInt(ms.length))
            val repl = scala.util.Random.nextInt(24).toString
            rx.replaceAllIn(s, m => if m.matched == target then repl else m.matched)

    def genMutation(s: String): Gen[String] =
        if s.isEmpty then Gen.const(mutAlphabet.mkString.take(3))
        else
            val ls = s.split("\n", -1)
            val lineIdx = Gen.choose(0, ls.length - 1)
            Gen.oneOf(
              for i <- Gen.choose(0, s.length); c <- Gen.oneOf(mutAlphabet)
              yield s.patch(i, c.toString, 0),
              for i <- Gen.choose(0, s.length - 1) yield s.patch(i, "", 1),
              for i <- Gen.choose(0, s.length - 1); c <- Gen.oneOf(mutAlphabet)
              yield s.patch(i, c.toString, 1),
              for i <- lineIdx yield (ls.take(i) ++ Array(ls(i)) ++ ls.drop(i)).mkString("\n"),
              for i <- lineIdx yield (ls.take(i) ++ ls.drop(i + 1)).mkString("\n"),
              for i <- lineIdx
              yield ls.updated(i, ls(i).stripPrefix("  ").stripPrefix(" ")).mkString("\n"),
              for i <- lineIdx yield ls.updated(i, "  " + ls(i)).mkString("\n"),
              if ls.length >= 2 then
                  for i <- Gen.choose(0, ls.length - 2)
                  yield ls.updated(i, ls(i + 1)).updated(i + 1, ls(i)).mkString("\n")
              else Gen.const(s),
              Gen.const(changeNumber(s))
            )

    val genMutatedProof: Gen[String] =
        for
            seed <- Gen.oneOf(seeds)
            n <- Gen.choose(1, 5)
            s <- (1 to n).foldLeft(Gen.const(seed))((g, _) => g.flatMap(genMutation))
        yield s

    val genAnyProofInput: Gen[String] =
        Gen.frequency((4, genMutatedProof), (3, genGarbageProof), (3, genSoup))

    // ── proof AST helpers ────────────────────────────────────────────────
    def pfLines(p: parser.PfScope): List[(LFormula, Rule)] =
        p.flatten.collect { case parser.Pf(c, r, _) => (c, r) }

    def allFormulas(p: parser.PfScope): List[LFormula] =
        p.flatten.collect { case parser.Pf(c, _, _) => c }

    def show(s: String): String =
        "\"" + s.replace("\n", "\\n").replace("\t", "\\t") + "\""
