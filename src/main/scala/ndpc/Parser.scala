package ndpc

import parsley.Parsley
import parsley.Result
import parsley.state.{RefMaker, forP}
import parsley.Parsley.{many, atomic, pure, eof}
import parsley.combinator.manyTill
import parsley.syntax.character.charLift
import parsley.character.item
import parsley.errors.combinator._
import parsley.debug._

import ndpc.expr.Formula._
import ndpc.expr.Rule.{Rule, ValidItem, Tick}
import ndpc.parsers.FormulaParser
import ndpc.parsers.Lexer.implicits.implicitSymbol
import ndpc.parsers.FormulaParser.lformula
import ndpc.parsers.RuleParser.rule
import ndpc.parsers.Utils._

import scala.io.Source
import scala.util.Try

object Parser {
    sealed trait Line[A <: ValidItem]
    case class Empty() extends Line[Int] {
        override def toString(): String = "<Empty Line>"
    }
    case class Comment(contents: String) extends Line[Int] {
        override def toString(): String = "<Comment>"
    }
    case class Pf[A <: ValidItem](
        val concl: LFormula[_],
        val rule: Rule[A],
        val trailingComment: Option[Comment]
    ) extends Line[A]

    case class PfScope[A <: ValidItem](var body: List[Line[A] | PfScope[A]])

    private class State(
        var indentLevel: Int,
        var cache: List[Line[Int]],
        var scopeStack: List[PfScope[Int]]
    ) {
        def getLast() = cache.last

        def addLine(line: Line[Int]) = {
            cache = cache :+ line
            this
        }
        def addLineToTree(line: Line[Int]): State = {
            // just append to current scope
            scopeStack.head.body = scopeStack.head.body :+ line
            this
        }

        def pushScopeWith(line: Line[Int]): State = {
            val newScope = PfScope(List(line))
            // add new scope to current scope
            scopeStack.head.body = scopeStack.head.body :+ newScope
            indentLevel += 2
            // add new scope to scope stack
            scopeStack = newScope :: scopeStack
            this
        }

        def popScopeWith(line: Line[Int]): State = {
            val t = scopeStack.tail
            t.head.body = t.head.body :+ line
            indentLevel -= 2
            scopeStack = t
            this
        }
    }

    case class UncheckedProof(main: PfScope[Int], lines: List[Line[Int]])

    private def emptyState() =
        State(0, List(), List(PfScope(List())))

    // format: off
    private val p: Parsley[UncheckedProof] = emptyState().makeRef { state =>
        val lineComment = ("--" ~> manyTill(item, '\n'))
            .map(_.mkString)
            .map(Comment.apply)

        // in linewise parsers we only append to the lines list since we don't know our indent
        val comment = state.update(
            lineComment.map { c => (s: State) =>
                s.addLine(c)
            }
        ) ~> state.get.map(_.getLast())

        val empty = state.update(
            manyTill(" " <|> "\t", '\n').map { _ => (s: State) =>
                s.addLine(Empty())
            }
        ).as(Empty())

        val pf: Parsley[Pf[Int]] = 
            state.update((
                // TODO(xiaoshihou514): use lexeme
                (lformula <~ spc) <~>
                ("[" ~> rule <~ spc <~ "]") <~>
                (lineComment.map(Option.apply) <|> '\n'.as(None))
            )
            .map { (res: ((LF_, Rule[Int]), Option[Comment])) =>
                Pf(res._1._1, res._1._2, res._2)
            }
            .map { pf => (s: State) =>
                s.addLine(pf)
            }
        ) ~> state.get.map(_.getLast().asInstanceOf[Pf[Int]])

        many(
            state.update((
                // empty line / comments, don' change the indents
                (
                    state.gets(_.indentLevel) <~> (
                    atomic(empty).label("empty line") <|>
                    atomic(spc ~> lineComment).label("line comment")
                )) <|>
                // a line of proof has to have the correct indents
                ((
                    atomic(
                        forP[Int](state.gets(_.indentLevel + 2), pure(_ > 0), pure(_ - 1)) {
                            ' '
                        } ~> state.gets(_.indentLevel + 2)
                    ).label("more indent than last line") <|>
                    atomic(
                        forP[Int](state.gets(_.indentLevel), pure(_ > 0), pure(_ - 1)) { 
                            ' '
                        } ~> state.gets(_.indentLevel)
                    ).label("same indent as last line") <|>
                    atomic(
                        forP[Int](state.gets(_.indentLevel - 2), pure(_ > 0), pure(_ - 1)) {
                            ' '
                        } ~> state.gets(_.indentLevel - 2)
                    ).label("less indent than last line")
                ) <~> pf.label("line of proof"))
                ).map { (res: (Int, Line[Int])) => (s: State) =>
                    res match {
                        // these are not indent agnostic
                        case (_ , nonpf @ (Comment(_) | Empty())) =>
                            s.addLineToTree(nonpf)
                        case (same, pf @ Pf[Int](_, rule, _)) if same == s.indentLevel => 
                            rule match {
                                // pop scope if this line is a tick
                                case Tick(_) => s.popScopeWith(pf)
                                case _ => s.addLineToTree(pf)
                            }
                        case (indented, pf) if indented == s.indentLevel + 2 => 
                            s.pushScopeWith(pf)
                        case (deindented, pf) if deindented == s.indentLevel - 2 => 
                            s.popScopeWith(pf)
                        case _ => ??? // unreachable
                    }
                }
        )) ~> state.gets { s => UncheckedProof(s.scopeStack.last, s.cache) }
    } <~ eof
    // format: on

    def parse(input: String) = p.parse(input)
}
