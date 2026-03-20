package ndpc.web

import scala.scalajs.js
import com.raquo.laminar.api.L.*
import org.scalajs.dom
import ndpc.web.components.*
import typings.codemirrorView.mod.{EditorView => CmEditorView}

object Main:
    def main(args: Array[String]): Unit =
        Theme.initBodyClass()

        val errors = Var(List.empty[CheckError])
        val viewVar = Var[Option[CmEditorView]](None)

        val editorEl = div(
          cls := "editor-container",
          onMountCallback { ctx =>
              val view = Editor.create(ctx.thisNode.ref, errors)
              viewVar.set(Some(view))
          }
        )

        // ── Derived signals ──────────────────────────────────────────

        val statusText = errors.signal.map { es =>
            if es.isEmpty then "🎉" else "❌"
        }

        def withView(f: CmEditorView => Unit): Unit =
            viewVar.now().foreach(f)

        // ── Toolbar ──────────────────────────────────────────────────
        val toolbar = div(
          cls := "toolbar",
          span(cls := "toolbar-title", "ndpc playground"),
          span(child.text <-- statusText),
          label(
            cls := "theme-switch",
            title := "Toggle light/dark theme",
            input(
              typ := "checkbox",
              checked <-- Theme.isDarkVar.signal,
              onClick --> { _ => withView(Theme.toggle) }
            ),
            span(cls := "theme-switch-slider")
          ),
          select(
            cls := "toolbar-select",
            option(value := "", disabled := true, selected := true, "Load example…"),
            Examples.all.map { (name, text) => option(value := text, name) },
            onChange --> { e =>
                val sel = e.target.asInstanceOf[dom.html.Select]
                val text = sel.value
                withView { view =>
                    view.asInstanceOf[js.Dynamic]
                        .dispatch(
                          js.Dynamic.literal(
                            changes = js.Dynamic.literal(
                              from = 0,
                              to = view.state.doc.asInstanceOf[js.Dynamic].length,
                              insert = text
                            )
                          )
                        )
                }
                sel.selectedIndex = 0
            }
          )
        )

        // ── Info panel ───────────────────────────────────────────────
        val infoPanel = div(
          cls := "info-panel",
          children <-- errors.signal.map {
              case Nil =>
                  List(div(cls := "info-ok", "✓ All proofs are correct!"))
              case errs =>
                  errs.map { err =>
                      div(
                        div(
                          cls := "info-error-location",
                          s"Line ${err.line}, col ${err.col}"
                        ),
                        div(cls := "info-error-message", err.message)
                      )
                  }
          }
        )

        // ── Layout ───────────────────────────────────────────────────
        renderOnDomContentLoaded(
          dom.document.getElementById("app"),
          div(
            cls := "app-root",
            toolbar,
            div(cls := "content-row", editorEl, infoPanel)
          )
        )
