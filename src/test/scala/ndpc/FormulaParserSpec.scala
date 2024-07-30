package ndpc

import ndpc.parsers.FormulaParser._
import ndpc.expr.Formula._
import scala.language.implicitConversions // yes, I know what I am doing

given Conversion[String, LTerm.Variable] with
    def apply(s: String): LTerm.Variable = LTerm.Variable(s)
def Fn = Function
def P = Predicate

class FormulaParserSpec extends UnitSpec {
    // "A variable" should "be any non keyword string" in {
    //     assert(variable.parse("valid").get === LTerm.Variable("valid"))
    //     assert(variable.parse("abc ").get === LTerm.Variable("abc"))
    // }

    // "A function application" should "be a function followed by (, some lterms and a )" in {
    //     val example = LTerm.FuncAp(
    //       Fn("foo", 3),
    //       List("x", "y", "z")
    //     )
    //     assert(funcAp.parse("foo(x, y, z)").get === example)
    //     assert(funcAp.parse("foo  (x , y,z  )").get === example)
    //     val nested1 = LTerm.FuncAp(
    //       Fn("bar", 1),
    //       List(example)
    //     )
    //     val nested2 = LTerm.FuncAp(
    //       Fn("bar", 2),
    //       List(example, ("kk"))
    //     )
    //     val nested3 = LTerm.FuncAp(
    //       Fn("baz", 6),
    //       List(nested2, "h", "j", "k", "l", nested1)
    //     )
    //     assert(funcAp.parse("bar (foo(x,y,z))").get === nested1)
    //     assert(funcAp.parse("bar (foo(x,y,z)    , kk)").get === nested2)
    //     assert(
    //       funcAp
    //           .parse(
    //             "baz (bar (foo(x,y,z)    , kk), h, j  ,  k,l, bar (foo(x,y  ,z)))"
    //           )
    //           .get === nested3
    //     )
    // }

    // "An LTerm" should "be a funcap or a variable" in {
    //     assert(lterm.parse("par").get === LTerm.Variable("par"))
    //     assert(lterm.parse("sley").get === LTerm.Variable("sley"))
    //     val example = LTerm.FuncAp(
    //       Fn("foo", 3),
    //       List("x", "y", "z")
    //     )
    //     assert(lterm.parse("foo(x, y, z)").get === example)
    //     assert(lterm.parse("foo  (x , y,z  )").get === example)
    //     val nested1 = LTerm.FuncAp(
    //       Fn("bar", 1),
    //       List(example)
    //     )
    //     val nested2 = LTerm.FuncAp(
    //       Fn("bar", 2),
    //       List(example, ("kk"))
    //     )
    //     val nested3 = LTerm.FuncAp(
    //       Fn("baz", 6),
    //       List(nested2, "h", "j", "k", "l", nested1)
    //     )
    //     assert(lterm.parse("bar (foo(x,y,z))").get === nested1)
    //     assert(lterm.parse("bar (foo(x,y,z)    , kk)").get === nested2)
    //     assert(
    //       lterm
    //           .parse(
    //             "baz (bar (foo(x,y,z)    , kk), h, j  ,  k,l, bar (foo(x,y  ,z)))"
    //           )
    //           .get === nested3
    //     )
    // }

    // "A predAp" should "be a predicate applied to multiple lterms" in {
    //     val example1 = LFormula.PredAp(
    //       P("foo", 3),
    //       List("x", "y", "z")
    //     )
    //     assert(predAp.parse("foo ( x, y, z )").get === example1)
    //     assert(lformula.parse("foo ( x, y, z )").get === example1)
    //     val example2 = LFormula.PredAp(
    //       P("foo", 2),
    //       List(
    //         LTerm.FuncAp(
    //           Fn("bar", 2),
    //           List("ss", "l")
    //         ),
    //         "w"
    //       )
    //     )
    //     assert(predAp.parse("foo( bar(ss,l), w)").get === example2)
    //     assert(lformula.parse("foo( bar(ss,l), w)").get === example2)
    //     val example3 = LFormula.PredAp(
    //       P("foo", 2),
    //       List(
    //         LTerm.FuncAp(
    //           Fn("bar", 2),
    //           List(
    //             "ss",
    //             LTerm.FuncAp(
    //               Fn("wacc", 4),
    //               List("w", "a", "c", "c")
    //             )
    //           )
    //         ),
    //         "w"
    //       )
    //     )
    //     assert(
    //       predAp
    //           .parse("foo( bar(ss,wacc   (w  , a , c,c)  ), w)")
    //           .get === example3
    //     )
    //     assert(
    //       lformula
    //           .parse("foo( bar(ss,wacc   (w  , a , c,c)  ), w)")
    //           .get === example3
    //     )
    //     val sugar = LFormula.PredAp(
    //       P("foo", 0),
    //       List()
    //     )
    //     assert(predAp.parse("foo^bar").get === sugar)
    // }

    // "eq" should "be an lterm = an lterm" in {
    //     val example1 = LFormula.Eq("a", "bb")
    //     assert(equ.parse("a   = bb").get === example1)
    //     assert(lformula.parse("a   = bb").get === example1)
    //     val example2 = LFormula.Eq(
    //       "x",
    //       LTerm.FuncAp(
    //         Fn("wuu", 2),
    //         List("a", "wa")
    //       )
    //     )
    //     assert(equ.parse("x=  wuu (  a, wa)").get === example2)
    //     assert(lformula.parse("x=  wuu (  a, wa)").get === example2)
    //     val example3 = LFormula.Eq(
    //       LTerm.FuncAp(
    //         Fn("jkjk", 0),
    //         List()
    //       ),
    //       LTerm.FuncAp(
    //         Fn("u", 1),
    //         List(
    //           LTerm.FuncAp(
    //             Fn("qo", 2),
    //             List("j", "w")
    //           )
    //         )
    //       )
    //     )
    //     assert(equ.parse("jkjk ()= u(qo(j,w))").get === example3)
    //     assert(lformula.parse("jkjk ()= u(qo(j,w))").get === example3)
    // }

    // "Truth and falsity" should "be a single char T/F" in {
    //     assert(truth.parse("T ^ fff").get === LFormula.Truth)
    //     assert(truth.parse("T").get === LFormula.Truth)
    //     assert(truth.parse("TasVar").isFailure)
    //     assert(falsity.parse("Fstart").isFailure)
    // }

    // "An atom in a LFormula" should "be a predAp / T / F" in {
    //     val atom1 = LFormula.PredAp(
    //       P("TStartFunc", 3),
    //       List(
    //         LTerm.FuncAp(
    //           Fn("FStartFunc", 2),
    //           List("a", "b")
    //         ),
    //         LTerm.FuncAp(Fn("fs", 0), List()),
    //         "j"
    //       )
    //     )
    //     assert(
    //       atom.parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j)").get === atom1
    //     )
    // }

    // "An lformula" should "be connectives + lfromula / forall(lformula) / exists(lformula)" in {
    //     val atomWithBrackets = LFormula.PredAp(
    //       P("TStartFunc", 3),
    //       List(
    //         LTerm.FuncAp(
    //           Fn("FStartFunc", 2),
    //           List("a", "b")
    //         ),
    //         LTerm.FuncAp(Fn("fs", 0), List()),
    //         "j"
    //       )
    //     )
    //     assert(
    //       lformula
    //           .parse("(  TStartFunc (FStartFunc(a,b)  ,   fs(), j))")
    //           .get === atomWithBrackets
    //     )

    //     val connectives_1 = LFormula.Not(atomWithBrackets)
    //     assert(
    //       lformula
    //           .parse("~ (  TStartFunc (FStartFunc(a,b)  ,   fs(), j))")
    //           .get === connectives_1
    //     )
    //     assert(
    //       lformula
    //           .parse("~(  TStartFunc (FStartFunc(a,b)  ,   fs(), j))")
    //           .get === connectives_1
    //     )

    //     val connectives_2 = LFormula.And(
    //       atomWithBrackets,
    //       LFormula.Truth
    //     )
    //     assert(
    //       lformula
    //           .parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j) ^T")
    //           .get === connectives_2
    //     )
    //     assert(
    //       lformula
    //           .parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j) ^  T")
    //           .get === connectives_2
    //     )
    //     assert(
    //       lformula
    //           .parse("TStartFunc (FStartFunc(a,b)  ,   fs(), j)^  T")
    //           .get === connectives_2
    //     )

    //     val connectives_3 = LFormula.Or(
    //       LFormula.Falsity,
    //       atomWithBrackets
    //     )
    //     assert(
    //       lformula
    //           .parse("F/TStartFunc (FStartFunc(a,b)  ,   fs(), j)")
    //           .get === connectives_3
    //     )

    //     val connectives_4 = LFormula.Implies(
    //       atomWithBrackets,
    //       atomWithBrackets
    //     )
    //     assert(
    //       lformula
    //           .parse(
    //             "(TStartFunc(FStartFunc(a,b),fs(),j)) ->  TStartFunc (FStartFunc(a,b)  ,   fs(), j)"
    //           )
    //           .get === connectives_4
    //     )

    //     val connectives_5 = LFormula.Equiv(
    //       LFormula.Truth,
    //       LFormula.Falsity
    //     )
    //     assert(lformula.parse("(((((T)))))    <->  (F)").get === connectives_5)

    //     val connectives_6 = LFormula.And(
    //       LFormula.Implies(
    //         LFormula.PredAp(P("p", 0), List()),
    //         LFormula.PredAp(P("q", 0), List())
    //       ),
    //       LFormula.Implies(
    //         LFormula.Not(LFormula.PredAp(P("p", 0), List())),
    //         LFormula.PredAp(P("r", 0), List())
    //       )
    //     )
    //     assert(lformula.parse("(p -> q) ^ (~p -> r)").get === connectives_6)

    //     val connectives_7 = LFormula.And(
    //       LFormula.Eq("p", "q"),
    //       LFormula.Implies(
    //         LFormula.Not(LFormula.PredAp(P("p", 0), List())),
    //         LFormula.PredAp(P("r", 0), List())
    //       )
    //     )
    //     assert(
    //       lformula.parse("(p=    q   )^    ( ~  p-> r  )").get === connectives_7
    //     )

    //     val forall = LFormula.Forall(
    //       List("p", "q"),
    //       connectives_7
    //     )
    //     assert(
    //       lformula
    //           .parse("forall p q. ((p=    q   )^    ( ~  p-> r  ))")
    //           .get === forall
    //     )

    //     val exists = LFormula.Forall(
    //       List("𝝓", "φ"),
    //       LFormula.Exists(
    //         List("A"),
    //         LFormula.Or(
    //           LFormula.PredAp(
    //             P("foo", 3),
    //             List("𝝓", "φ", "A")
    //           ),
    //           LFormula.Exists(
    //             List("B"),
    //             LFormula.PredAp(
    //               P("bar", 2),
    //               List("𝝓", "B")
    //             )
    //           )
    //         )
    //       )
    //     )
    //     assert(
    //       lformula
    //           .parse(
    //             "forall 𝝓 φ. (exists A. ( foo(𝝓 ,φ ,A) / (exists B. (bar(𝝓, B)))))"
    //           )
    //           .get === exists
    //     )

    //     val precedence = LFormula.Implies(
    //       LFormula.And(
    //         LFormula.PredAp(P("a", 0), List()),
    //         LFormula.PredAp(P("b", 0), List())
    //       ),
    //       LFormula.Equiv(
    //         LFormula.PredAp(P("a", 0), List()),
    //         LFormula.PredAp(P("a", 0), List())
    //       )
    //     )
    //     assert(lformula.parse("a^b->(a<->a)").get === precedence)

    // }

    // "Any LFormula" should "be parsed correctly" in {
    //     val `all green dragons can fly` = LFormula.Forall(
    //       List("x"),
    //       LFormula.Implies(
    //         LFormula.And(
    //           LFormula.PredAp(P("dragon", 1), List("x")),
    //           LFormula.PredAp(P("green", 1), List("x"))
    //         ),
    //         LFormula.PredAp(P("fly", 1), List("x"))
    //       )
    //     )
    //     assert(
    //       lformula
    //           .parse("forall x. (dragon(x) ^ green(x) -> fly(x))")
    //           .get === `all green dragons can fly`
    //     )
    // }
}
