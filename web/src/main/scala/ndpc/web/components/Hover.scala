package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import typings.codemirrorView.mod.EditorView
import ndpc.web.Docs

// Namespace import gives access to hoverTooltip without dealing with its complex
// Scala type signature — we pass a plain js.Function3 instead.
@js.native @JSImport("@codemirror/view", JSImport.Namespace)
private object CmViewNS extends js.Object

object Hover:
    private val cmView = CmViewNS.asInstanceOf[js.Dynamic]

    private val tooltipSource: js.Function3[EditorView, Double, js.Any, js.Any] =
        (view, pos, _side) =>
            val state = view.state.asInstanceOf[js.Dynamic]
            val word  = state.wordAt(pos)
            if (word == null) null.asInstanceOf[js.Any]
            else
                val text = state.sliceDoc(word.from, word.to).asInstanceOf[String]
                Docs.docs.get(text) match
                    case None => null.asInstanceOf[js.Any]
                    case Some(description) =>
                        js.Dynamic.literal(
                            pos    = word.from,
                            end    = word.to,
                            above  = true,
                            create = (() =>
                                val container = dom.document.createElement("div").asInstanceOf[js.Dynamic]
                                container.className = "cm-tooltip-ndpc"
                                val bold = dom.document.createElement("b").asInstanceOf[js.Dynamic]
                                bold.textContent = text
                                container.appendChild(bold)
                                val rest = dom.document.createTextNode(s": $description")
                                container.appendChild(rest)
                                js.Dynamic.literal(dom = container)
                            ): js.Function0[js.Dynamic]
                        )

    val extension: js.Any = cmView.hoverTooltip(tooltipSource)

