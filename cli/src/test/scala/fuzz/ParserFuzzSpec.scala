package ndpc.fuzz

import org.scalacheck.Prop
import ndpc.frontend.parser
import ndpc.frontend.pretty.*
import parsley.{Failure, Success}

/** Parser fuzzing: the parser must never throw, and printing must roundtrip. */
class ParserFuzzSpec extends FuzzSpec:

    property("BUG-HUNT parse: never throws on arbitrary string soup") {
        checkProp("parse(soup)", Prop.forAll(FuzzGens.genSoup)(parseNoThrow))
    }

    property("BUG-HUNT parse: never throws on mutated real proofs") {
        checkProp("parse(mutated)", Prop.forAll(FuzzGens.genMutatedProof)(parseNoThrow))
    }

    property("BUG-HUNT parse: never throws on generated garbage proofs") {
        checkProp("parse(garbage)", Prop.forAll(FuzzGens.genGarbageProof)(parseNoThrow))
    }

    property("BUG-HUNT pretty: printed formulas reparse to the same tree") {
        checkProp(
          "formula roundtrip",
          Prop.forAll(FuzzGens.genLFormula) { f =>
              FuzzGens.parseFormula(f.pretty) match
                  case Success(g) =>
                      g == f || FuzzGens.knownPrecedenceBug(f)
                  case Failure(_) =>
                      FuzzGens.knownPrecedenceBug(f)
          }
        )
    }

    property("BUG-HUNT pretty: weird formulas never crash printer or formula parser") {
        checkProp(
          "weird formula",
          Prop.forAll(FuzzGens.genWeirdFormula) { f =>
              val printed = f.pretty
              FuzzGens.parseFormula(printed)
              true
          }
        )
    }
