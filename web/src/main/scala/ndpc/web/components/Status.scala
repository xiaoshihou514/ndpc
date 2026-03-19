package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import com.raquo.laminar.api.L.Var
import ndpc.web.CheckError

@js.native @JSImport("@codemirror/view", JSImport.Namespace)
private object CmViewStatus extends js.Object

object Status:
    private val viewNS = CmViewStatus.asInstanceOf[js.Dynamic]

    /** Returns a CM6 ViewPlugin extension that writes the error list into `errors` on each
      * doc change. Laminar observes the Var to derive status display and the info panel.
      */
    def build(errors: Var[List[CheckError]]): js.Any =
        viewNS.ViewPlugin.define(
            (
                (_view: js.Dynamic) =>
                    js.Dynamic.literal(
                        update = (
                            (update: js.Dynamic) =>
                                if update.docChanged.asInstanceOf[Boolean] then
                                    val text = update.state.doc.toString().asInstanceOf[String]
                                    errors.set(ndpc.web.Checker.check(text))
                        ): js.Function1[js.Dynamic, Unit]
                    )
            ): js.Function1[js.Dynamic, js.Dynamic]
        )
