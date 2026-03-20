package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import com.raquo.laminar.api.L.Var
import typings.codemirrorView.mod.{EditorView, EditorViewConfig}

@js.native @JSImport("codemirror", JSImport.Namespace)
private object CmSetup extends js.Object

@js.native @JSImport("@codemirror/view", JSImport.Namespace)
private object CmViewDyn extends js.Object

@js.native @JSImport("@codemirror/commands", JSImport.Namespace)
private object CmCmds extends js.Object

object Editor:
    private val setup = CmSetup.asInstanceOf[js.Dynamic]
    private val view = CmViewDyn.asInstanceOf[js.Dynamic]
    private val cmds = CmCmds.asInstanceOf[js.Dynamic]

    /** Create and mount the CM6 editor into `parent`. Writes errors into `errors`. */
    def create(parent: dom.Element, errors: Var[List[ndpc.web.CheckError]]): EditorView =
        val statusExt = Status.build(errors)
        new EditorView(
          js.Dynamic
              .literal(
                doc = Examples.default,
                extensions = js.Array(
                  setup.minimalSetup,
                  view.lineNumbers(),
                  view.highlightActiveLine(),
                  Theme.initialExtension.asInstanceOf[js.Any],
                  Highlight.extension.asInstanceOf[js.Any],
                  Lint.extension.asInstanceOf[js.Any],
                  Folding.extension.asInstanceOf[js.Any],
                  Hover.extension,
                  Completion.extension,
                  IndentLines.extension.asInstanceOf[js.Any],
                  statusExt
                ),
                parent = parent
              )
              .asInstanceOf[EditorViewConfig]
        )
