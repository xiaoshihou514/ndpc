package ndpc

import parsley.Parsley
import parsley.state.RefMaker
import parsley.Parsley.{many, atomic}
import parsley.combinator.manyTill
import parsley.character.whitespace
import parsley.syntax.character.charLift
import parsley.character.item
import parsley.debug._

import ndpc.expr.Formula._
import ndpc.expr.Rule.{Rule, Special}
import ndpc.parsers.FormulaParser
import ndpc.parsers.Lexer.implicits.implicitSymbol
import ndpc.parsers.FormulaParser.lformula
import ndpc.parsers.RuleParser.rule
import ndpc.parsers.Utils._

object Parser {
    sealed trait Line
    case class Empty() extends Line
    case class Comment(contents: String) extends Line
    case class Pf(
        concl: LFormula[_],
        rule: Rule[BigInt],
        trailingComment: Option[Comment]
    ) extends Line

    case class PfScope(var body: List[Line | PfScope])

    class State(
        var indentLevel: Int,
        var cache: List[Line],
        var scopeStack: List[PfScope]
    ) {
        def getLast() = cache.last

        def addLine(line: Line) = {
            cache = cache :+ line
            this
        }
        def addLineToTree(line: Line): State = {
            // just append to current scope
            scopeStack.head.body = scopeStack.head.body :+ line
            this
        }

        def pushScopeWith(line: Line): State = {
            val newScope = PfScope(List(line))
            // add new scope to current scope
            scopeStack.head.body = scopeStack.head.body :+ newScope
            indentLevel += 2
            // add new scope to scope stack
            scopeStack = newScope :: scopeStack
            this
        }

        def popScopeWith(line: Line): State = {
            val t = scopeStack.tail
            t.head.body = t.head.body :+ line
            indentLevel -= 2
            scopeStack = t
            this
        }
    }

    case class UncheckedProof(main: PfScope, lines: List[Line])

    private val emptyState =
        State(0, List(), List(PfScope(List())))

    // format: off
    val p: Parsley[UncheckedProof] = emptyState.makeRef { state =>
        val lineComment = ("--" ~> manyTill(item, '\n'))
            .map(_.mkString)
            .map(Comment.apply)

        // in linewise parsers we only append to the lines list since we don't know our indent
        val comment = state.update(
            lineComment
            .map { c => (s: State) =>
                s.addLine(c)
            }
        ) ~> state.get.map(_.getLast())

        val empty = state.update(
            manyTill(whitespace, '\n').void
            .map { e => (s: State) =>
                s.addLine(Empty())
            }
        ).as(Empty())

        val pf = 
            state.update((
                // TODO(xiaoshihou514): use lexeme
                (lformula <~ spc) <~>
                ("[" ~> rule <~ spc <~ "]") <~>
                (lineComment.map(Option.apply) <|> '\n'.as(None))
            )
            .map { (res: ((LF_, Rule[BigInt]), Option[Comment])) =>
                Pf(res._1._1, res._1._2, res._2)
            }
            .map { pf => (s: State) =>
                s.addLine(pf)
            }
        ) ~> state.get.map(_.getLast())

        many(
            state.update((
                many(' ').map(_.length) <~> 
                (
                    comment <|> 
                    empty <|> 
                    pf
                )).map { (res: (Int, Line)) => (s: State) =>
                    res match {
                        // these are not indent agnostic
                        case (_ , nonpf @ (Comment(_) | Empty())) =>
                            s.addLineToTree(nonpf)
                        case (same, pf @ Pf(_, rule, _)) if same == s.indentLevel => 
                            rule match {
                                // TODO: I hope there's a better way of doing this
                                // pop scope if this line is a tick
                                case Rule.Builtin(Special.Tick(_)) => s.popScopeWith(pf)
                                case _ => s.addLineToTree(pf)
                            }
                        case (indented, pf) if indented == s.indentLevel + 2 => 
                            s.pushScopeWith(pf)
                        case (deindented, pf) if deindented == s.indentLevel - 2 => 
                            s.popScopeWith(pf)
                        case (incorrect, _) => {
                            ???
                            // TODO: better errors
                        } 
                    }
                }
        )) ~> state.get.map { s => UncheckedProof(s.scopeStack.last, s.cache) }
    }
    // format: on

    def parse(input: String) = p.parse(input)
}
