package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import com.raquo.laminar.api.L.Var
import ndpc.web.CheckError

@js.native @JSImport("@codemirror/view", JSImport.Namespace)
private object CmViewStatus extends js.Object

object Status:
    private val viewNS = CmViewStatus.asInstanceOf[js.Dynamic]

    /** Returns a CM6 ViewPlugin extension that writes the error list into `errors` on each doc
      * change. Laminar observes the Var to derive status display and the info panel.
      */
    def build(errors: Var[List[CheckError]]): js.Any =
        viewNS.ViewPlugin.define(
          { (_: js.Dynamic) =>
              js.Dynamic.literal(
                update = { (update: js.Dynamic) =>
                    if update.docChanged.asInstanceOf[Boolean] then
                        errors.set(ndpc.web.Checker.check(update.state.doc.toString()))
                }
              )
          }
        )
