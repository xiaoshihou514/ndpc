package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native @JSImport("@codemirror/language", JSImport.Namespace)
private object CmLangIndent extends js.Object

@js.native @JSImport("@replit/codemirror-indentation-markers", JSImport.Namespace)
private object CmIndentMarkers extends js.Object

object IndentLines:
    private val lang    = CmLangIndent.asInstanceOf[js.Dynamic]
    private val markers = CmIndentMarkers.asInstanceOf[js.Dynamic]

    val extension: js.Array[js.Any] = js.Array(
        // Set the logical indent unit to 2 spaces
        lang.indentUnit.of("  "),
        // Visual indent guide lines
        markers.indentationMarkers(),
    )
