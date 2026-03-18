package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.JSConverters.*
import typings.codemirrorView.mod.EditorView
import ndpc.web.Checker

@js.native @JSImport("@codemirror/lint", JSImport.Namespace)
private object CmLint extends js.Object

object Lint:
    private val lint = CmLint.asInstanceOf[js.Dynamic]

    private val ndpcLinter: js.Any =
        lint.linter(
            ((view: EditorView) =>
                val text  = view.state.doc.asInstanceOf[js.Dynamic].toString().asInstanceOf[String]
                val errors = Checker.check(text)
                errors.map { err =>
                    val line = view.state.doc.line(err.line.toDouble)
                    val from = math.min(line.from.toInt + err.col - 1, line.to.toInt)
                    val to   = line.to.toInt
                    js.Dynamic.literal(
                        from     = from,
                        to       = to,
                        severity = "error",
                        message  = err.message
                    )
                }.toJSArray
            ): js.Function1[EditorView, js.Array[js.Dynamic]]
        )

    /** The combined extension: linter + gutter indicator. */
    val extension: js.Array[js.Any] = js.Array(ndpcLinter, lint.lintGutter())
