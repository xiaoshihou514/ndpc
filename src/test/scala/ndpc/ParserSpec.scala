package ndpc

class ParserSpec extends UnitSpec {
    "A document" should "be well-indented" in {
        import parsley.Parsley
        import parsley.Parsley.{many, atomic, pure}
        import parsley.character.{char, item, stringOfMany, letter, digit}
        import parsley.combinator.sepBy
        import parsley.state.{RefMaker, Ref, StateCombinators}
        import parsley.syntax.character.{charLift, stringLift}
        import parsley.debug._

        sealed trait Pf
        case class Stmt(body: String, ref: List[String]) extends Pf
        case class Scope(body: List[Pf]) extends Pf

        case class State(
            level: Int,
            lines: List[String],
            scope: List[Scope]
        )
        val number = digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)

        // format: off
        val p: Parsley[Scope] = State(0, List(), List(Scope(List()))).makeRef { (state: Ref[State]) =>
            val stmt: Parsley[Stmt] = (
                many(letter).map(_.mkString).debug("many letters").flatMap { str =>
                    state.update { s =>
                        s.copy(lines = s.lines :+ str)
                    }
                    pure(str)
                }.debug("many letter fmap") <~ '('.debug("left br") <~> state.get.flatMap { s =>
                    sepBy(number, ", ").map { xs =>
                        println(s.lines)
                        xs.map { (x: Int) => s.lines(x - 1) }
                    } <~ ')'
                }.debug("brackets")
            ).map { res => Stmt(res._1, res._2) }.debug("stmt")
            val scope: Parsley[Scope] = 
                many((
                    many(' ').map(_.length) <~> stmt <~ '\n'
                ).map { res => state.get.flatMap { s =>
                    val indented = s.level + 4
                    val deindented = s.level - 4
                    res._1 match {
                        case (s.level) => {
                            val sc = s.scope
                            state.set(s.copy(scope = sc.head.copy(body = sc.head.body :+ res._2) :: sc.tail))
                        }
                        case (indented) => {
                            // new scope
                            val newScope = Scope(List(res._2))
                            val sc = s.scope
                            state.set(s.copy(
                                level = indented,
                                scope = newScope :: sc.head.copy(body = sc.head.body :+ newScope) :: sc.tail
                            ))
                        } 
                        case (deindented) => {
                            // prev scope
                            val t = s.scope.tail
                            state.set(s.copy(
                                level = deindented,
                                scope = t.head.copy(body = t.head.body :+ res._2) :: t.tail
                            ))
                        }
                    }
                }}) ~> state.get.map { s => s.scope.head }
            scope
        }
        println(p.parse("boo()\nbar(1)\n"))
    }
}
