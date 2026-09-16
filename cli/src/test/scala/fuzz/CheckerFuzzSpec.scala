package ndpc.fuzz

import org.scalacheck.Prop
import ndpc.frontend.{Checker, Formatter}
import ndpc.frontend.parser
import parsley.{Failure, Success}

/** Checker + formatter fuzzing. */
class CheckerFuzzSpec extends FuzzSpec:

    property("BUG-HUNT checker: never throws on any input that parses") {
        checkProp(
          "checker no-throw",
          Prop.forAll(FuzzGens.genAnyProofInput) { s =>
              guarded("CHECKER", s) {
                  parser.parse(s) match
                      case Success(ast) =>
                          Checker.checkParsed(ast)
                          true
                      case Failure(_) => true
              }
          }
        )
    }

    property("BUG-HUNT format: output reparses and keeps every proof line") {
        checkProp(
          "format roundtrip",
          Prop.forAll(FuzzGens.genAnyProofInput) { s =>
              guarded("FORMAT PATH", s) {
                  parser.parse(s) match
                      case Success(ast) =>
                          val formatted = Formatter.formatPure(ast)
                          parser.parse(formatted) match
                              case Success(re) =>
                                  FuzzGens.pfLines(ast.main) == FuzzGens.pfLines(re.main)
                              case Failure(_) =>
                                  System.err.println(
                                    s"FORMATTED OUTPUT DOES NOT REPARSE: " +
                                        FuzzGens.show(formatted)
                                  )
                                  false
                      case Failure(_) => true
              }
          }
        )
    }

    property("BUG-HUNT format: formatting is idempotent") {
        checkProp(
          "format idempotent",
          Prop.forAll(FuzzGens.genAnyProofInput) { s =>
              guarded("FORMAT PATH", s) {
                  parser.parse(s) match
                      case Success(ast) =>
                          val once = Formatter.formatPure(ast)
                          parser.parse(once) match
                              case Success(re) => Formatter.formatPure(re) == once
                              case Failure(_) =>
                                  System.err.println(
                                    s"FORMATTED OUTPUT DOES NOT REPARSE: " +
                                        FuzzGens.show(once)
                                  )
                                  false
                      case Failure(_) => true
              }
          }
        )
    }

    property("BUG-HUNT valid: checker accepts every proof built valid-by-construction (AST)") {
        checkProp(
          "valid AST accepted",
          Prop.forAll(ValidProof.gen) { vp =>
              Checker.checkParsed(vp.ast) match
                  case Success(_) => true
                  case Failure(err) =>
                      System.err.println(
                        s"REJECTED VALID PROOF:\n${vp.text}\nerror: $err"
                      )
                      false
          }
        )
    }

    property("BUG-HUNT valid: checker accepts the printed form of every valid proof") {
        checkProp(
          "valid text accepted",
          Prop.forAll(ValidProof.gen) { vp =>
              Checker.checkedFromString(vp.text) match
                  case Success(_) => true
                  case Failure(err) =>
                      System.err.println(
                        s"REJECTED PRINTED VALID PROOF:\n${vp.text}\nerror: $err"
                      )
                      false
          }
        )
    }
