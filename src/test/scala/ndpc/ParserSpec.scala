package ndpc

class ParserSpec extends UnitSpec {
    "A document" should "be well-indented" in {
        import parsley.Parsley
        import parsley.Parsley.{many, atomic, pure}
        import parsley.character.{char, item, stringOfMany, letter, digit}
        import parsley.combinator.sepBy
        import parsley.state.{RefMaker, Ref, StateCombinators}
        import parsley.syntax.character.{charLift, stringLift}

        sealed trait Pf
        case class Stmt(body: String, ref: List[String]) extends Pf
        case class Scope(var body: List[Pf]) extends Pf

        case class State(
            level: Int,
            lines: List[String],
            scope: List[Scope]
        )
        val number = digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)

        // format: off
        val p: Parsley[Scope] = State(0, List(), List(Scope(List()))).makeRef { (state: Ref[State]) =>
            val stmt: Parsley[Stmt] = (
                state.update(
                    many(letter).map(_.mkString).map { str => (s: State) =>
                            s.copy(lines = s.lines :+ str)
                    }
                ) ~> '(' ~> state.get.map(_.lines.last) 
                <~> state.gets {
                    sepBy(number, ", ").map { xs => (s: State) =>
                        xs.map { (x: Int) => s.lines(x - 1) }
                    }
                } <~ ')'
            ).map { res => Stmt(res._1, res._2) }
            val scope: Parsley[Scope] = many(
                state.update((
                    many(' ').map(_.length) <~> stmt <~ '\n'
                ).map { res => (s: State) =>
                    val sc = s.scope
                    val i = res._1
                    if i == s.level then
                        s.copy(scope = sc.head.copy(body = sc.head.body :+ res._2) :: sc.tail)
                    else if i == s.level + 4 then
                        val newScope = Scope(List(res._2))
                        sc.head.body = sc.head.body :+ newScope
                        s.copy(
                            level = s.level + 4,
                            scope = newScope :: sc
                        )
                    else if i == s.level - 4 then
                        // prev scope
                        val t = sc.tail
                        t.head.body = t.head.body :+ res._2
                        s.copy(
                            level = s.level - 4,
                            scope = t
                        )
                    else
                        fail()  // TODO: better errors
                }
            )) ~> state.get.map { s => s.scope.last }
            scope
        }
        println(p.parse("boo()\nbar(1)\n    baz(1, 2)\nfizz()\nbuzz(4)\n").get)
    }
}
