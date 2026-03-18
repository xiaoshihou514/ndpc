package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom

@js.native @JSImport("@codemirror/view", JSImport.Namespace)
private object CmViewStatus extends js.Object

object Status:
    private val viewNS = CmViewStatus.asInstanceOf[js.Dynamic]

    /** Build a status indicator. Returns the CM6 extension and a <span> DOM element to append to
      * the toolbar. The extension is a ViewPlugin that updates the span on every doc change.
      */
    def build(): (js.Any, dom.html.Span) =
        val span = dom.document.createElement("span").asInstanceOf[dom.html.Span]
        span.className = "status-indicator"
        setStatus(span, ok = true, count = 0)

        // ViewPlugin.define takes (view => pluginValue); pluginValue may have update/destroy.
        // We run Checker.check() synchronously on every doc change for immediate feedback,
        // instead of reading diagnosticCount which only reflects the async linter state.
        val plugin = viewNS.ViewPlugin.define(
          (
              (_view: js.Dynamic) =>
                  js.Dynamic.literal(
                    update = (
                        (update: js.Dynamic) =>
                            if update.docChanged.asInstanceOf[Boolean] then
                                val text = update.state.doc.toString().asInstanceOf[String]
                                val count = ndpc.web.Checker.check(text).length
                                setStatus(span, ok = count == 0, count = count)
                    ): js.Function1[js.Dynamic, Unit]
                  )
          ): js.Function1[js.Dynamic, js.Dynamic]
        )

        (plugin, span)

    private def setStatus(span: dom.html.Span, ok: Boolean, count: Int): Unit =
        if ok then
            span.textContent = "🎉"
            span.className = "status-indicator status-ok"
        else
            span.textContent = s"✗ $count"
            span.className = "status-indicator status-error"
