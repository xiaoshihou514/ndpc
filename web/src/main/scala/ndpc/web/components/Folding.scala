package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import typings.codemirrorState.mod.EditorState

@js.native @JSImport("@codemirror/language", JSImport.Namespace)
private object CmLangFold extends js.Object

object Folding:
    private val lang = CmLangFold.asInstanceOf[js.Dynamic]

    // A foldService is a Facet value: (state, lineStart, lineEnd) => {from, to} | null
    private val ndpcFoldService: js.Any =
        lang.foldService.of(
            ((state: EditorState, lineStart: Double, _lineEnd: Double) =>
                val doc      = state.doc
                val openLine = doc.lineAt(lineStart)
                val openText = openLine.text
                val indent   = leadingSpaces(openText)

                // Scan forward to find the end of this indented block
                var endLineNum = openLine.number.toInt
                val totalLines = doc.lines.toInt
                var searching  = true
                while searching && endLineNum < totalLines do
                    val next = doc.line((endLineNum + 1).toDouble)
                    val nextText   = next.text
                    val nextIndent = leadingSpaces(nextText)
                    // A non-empty line with less-or-equal indentation ends the block
                    if nextText.trim().length > 0 && nextIndent <= indent then
                        searching = false
                    else
                        endLineNum += 1

                if endLineNum <= openLine.number.toInt then null.asInstanceOf[js.Any]
                else
                    val endLine = doc.line(endLineNum.toDouble)
                    js.Dynamic.literal(from = openLine.to, to = endLine.to)
            ): js.Function3[EditorState, Double, Double, js.Any]
        )

    private def leadingSpaces(s: String): Int =
        s.takeWhile(_ == ' ').length

    /** The combined extension: fold gutter + the ndpc fold service. */
    val extension: js.Array[js.Any] = js.Array(lang.foldGutter(), ndpcFoldService)
