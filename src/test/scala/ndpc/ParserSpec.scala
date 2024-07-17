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
        case class Stmt(body: String, ref: List[String]) extends Pf {
            override def toString() = s"$body(${ref.mkString(", ")})"
        }
        case class Scope(var body: List[Pf]) extends Pf {
            def toStringWithIndent(x: Int): String = body
                .map { pf =>
                    pf match {
                        case stmt @ Stmt(_, _) => " ".repeat(x * 4) + stmt
                        case scope @ Scope(_) => scope.toStringWithIndent(x + 1)
                    }
                }
                .mkString("\n")
            override def toString() = toStringWithIndent(0)
        }

        case class State(
            var level: Int,
            lines: List[String],
            var scope: List[Scope]
        ) {
            override def toString(): String =
                s"level: $level\n" + s"lookup: $lines\nscope:\n" + scope
                    .mkString("\n")
        }
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
                    val (i, sm) = res
                    if i == s.level then
                        sc.head.body = sc.head.body :+ sm
                    else if i == s.level + 4 then
                        // new scope
                        val newScope = Scope(List(sm))
                        sc.head.body = sc.head.body :+ newScope
                        s.level += 4
                        s.scope = newScope :: s.scope
                    else if i == s.level - 4 then
                        // prev scope
                        val t = sc.tail
                        t.head.body = t.head.body :+ sm
                        s.level -= 4
                        s.scope = t
                    else
                        fail()  // TODO: better errors
                    s
                }
            )) ~> state.get.map { s => s.scope.last }
            scope
        }
        // format: on
        val testStr = """
boo()
bar(1)
    baz(1, 2)
        fizz()
        buzz()
    h(1)
g(2)
n(3)
            """.trim() + "\n"
        val output = """
boo()
bar(boo)
    baz(boo, bar)
        fizz()
        buzz()
    h(boo)
g(bar)
n(baz)
            """.trim()
        assert(p.parse(testStr).get.toString() === output)
    }
}
