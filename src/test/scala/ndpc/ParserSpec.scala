package ndpc

class ParserSpec extends UnitSpec {
    "A document" should "be well-indented" in {
        import parsley.Parsley
        import parsley.Parsley.{eof, many, atomic}
        import parsley.character.{char, letter, item, stringOfMany}
        import parsley.errors.patterns.VerifiedErrors
        import parsley.position.pos
        import parsley.state.{RefMaker, Ref, StateCombinators}
        import parsley.syntax.lift

        sealed trait Pf
        case class Stmt(body: String) extends Pf
        case class Scope(body: List[Pf]) extends Pf

        case class State(level: Int, lines: List[Stmt])

        val indent = State(-1, List()).makeRef { (idt: Ref[State]) =>
            val stmt = stringOfMany(letter)
                .fillRef { stmt =>
                    idt.update { s =>
                        s.copy(lines = s.lines :+ Stmt(stmt.get))
                    }
                }
                <~ char('\n')
            ???
        }

        List.empty[Char]
            .makeRef { r1 =>
                item.fillRef { c =>
                    r1.set(c.get <::> r1.get)
                } ~> r1.get
            }
            .parse("a")
    }
}
