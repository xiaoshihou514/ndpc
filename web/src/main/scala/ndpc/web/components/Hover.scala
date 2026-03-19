package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import com.raquo.laminar.api.L.*
import typings.codemirrorView.mod.EditorView
import ndpc.web.Docs

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
                        // Build tooltip DOM with Laminar — access .ref for the raw node.
                        val container = div(
                            cls := "cm-tooltip-ndpc",
                            b(text),
                            s": $description",
                        )
                        js.Dynamic.literal(
                            pos    = word.from,
                            end    = word.to,
                            above  = true,
                            create = (() =>
                                js.Dynamic.literal(dom = container.ref)
                            ): js.Function0[js.Dynamic]
                        )

    val extension: js.Any = cmView.hoverTooltip(tooltipSource)
