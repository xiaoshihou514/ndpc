package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import typings.codemirrorView.mod.EditorView
import typings.codemirrorView.mod.EditorViewConfig

// Namespace imports for dynamic access — avoids wrestling with every generic type.
@js.native @JSImport("codemirror", JSImport.Namespace)
private object CmSetup extends js.Object

@js.native @JSImport("@codemirror/view", JSImport.Namespace)
private object CmViewDyn extends js.Object

@js.native @JSImport("@codemirror/commands", JSImport.Namespace)
private object CmCmds extends js.Object

object Editor:
    private val setup = CmSetup.asInstanceOf[js.Dynamic]
    private val view  = CmViewDyn.asInstanceOf[js.Dynamic]
    private val cmds  = CmCmds.asInstanceOf[js.Dynamic]

    private def allExtensions(statusExt: js.Any): js.Array[js.Any] =
        js.Array(
            // minimalSetup: history, drawSelection, default highlight (fallback), keymaps
            setup.minimalSetup,
            view.lineNumbers(),
            view.highlightActiveLine(),
            view.highlightActiveLineGutter(),
            // Tab key support
            view.keymap.of(js.Array(cmds.indentWithTab)),
            // Theme must come before highlight so our style overrides the fallback
            Theme.initialExtension.asInstanceOf[js.Any],
            // Custom ndpc extensions
            Highlight.extension.asInstanceOf[js.Any],
            Lint.extension.asInstanceOf[js.Any],
            Folding.extension.asInstanceOf[js.Any],
            Hover.extension,
            Completion.extension,
            IndentLines.extension.asInstanceOf[js.Any],
            statusExt,
        )

    /** Create the editor. Returns (view, statusSpan) so Main can mount the span. */
    def create(parent: dom.Element): (EditorView, org.scalajs.dom.html.Span) =
        val (statusExt, statusSpan) = Status.build()
        val view = new EditorView(
            js.Dynamic.literal(
                doc        = Examples.default,
                extensions = allExtensions(statusExt),
                parent     = parent,
            ).asInstanceOf[EditorViewConfig]
        )
        (view, statusSpan)
