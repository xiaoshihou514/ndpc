package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import com.raquo.laminar.api.L.Var
import typings.codemirrorState.mod.{Compartment, Extension}
import typings.codemirrorView.mod.EditorView

@js.native @JSImport("@codemirror/view", JSImport.Namespace)
private object CmViewTheme extends js.Object

object Theme:
    val compartment: Compartment = new Compartment()

    private val EditorViewJS = CmViewTheme.asInstanceOf[js.Dynamic].EditorView

    // VSCode Dark theme for the CM6 editor
    val dark: js.Any = EditorViewJS.theme(
      js.Dynamic.literal(
        "&" -> js.Dynamic.literal(
          backgroundColor = "#1f1f1f",
          color = "#d4d4d4",
          height = "100%",
          fontFamily = "'Cascadia Code', 'Fira Code', 'Menlo', monospace",
          fontSize = "14px"
        ),
        ".cm-content" -> js.Dynamic.literal(caretColor = "#d4d4d4"),
        ".cm-cursor" -> js.Dynamic.literal(borderLeftColor = "#d4d4d4"),
        ".cm-gutters" -> js.Dynamic.literal(
          backgroundColor = "#1f1f1f",
          color = "#808080",
          border = "none",
          borderRight = "1px solid #333"
        ),
        ".cm-activeLineGutter" -> js.Dynamic.literal(backgroundColor = "#282828"),
        ".cm-activeLine" -> js.Dynamic.literal(backgroundColor = "#282828"),
        ".cm-selectionBackground, ::selection" ->
            js.Dynamic.literal(backgroundColor = "#264f78"),
        ".cm-foldGutter" -> js.Dynamic.literal(color = "#808080"),
        ".cm-tooltip" -> js.Dynamic.literal(
          backgroundColor = "#252526",
          border = "1px solid #454545",
          color = "#cccccc",
          padding = "4px 8px",
          borderRadius = "4px"
        ),
        ".cm-tooltip-ndpc" -> js.Dynamic.literal(maxWidth = "400px", lineHeight = "1.5"),
        ".cm-lintRange-error" -> js.Dynamic.literal(
          backgroundImage =
              "url(\"data:image/svg+xml,%3Csvg%20xmlns%3D'http%3A//www.w3.org/2000/svg'%20width%3D'6'%20height%3D'3'%3E%3Cpath%20d%3D'M0%202%20L3%200%20L6%202'%20fill%3D'none'%20stroke%3D'%23f44747'%20stroke-width%3D'1.2'/%3E%3C/svg%3E\")",
          backgroundRepeat = "repeat-x",
          backgroundPosition = "bottom"
        )
      ),
      js.Dynamic.literal(dark = true)
    )

    // VSCode Light theme for the CM6 editor
    val light: js.Any = EditorViewJS.theme(
      js.Dynamic.literal(
        "&" -> js.Dynamic.literal(
          backgroundColor = "#ffffff",
          color = "#000000",
          height = "100%",
          fontFamily = "'Cascadia Code', 'Fira Code', 'Menlo', monospace",
          fontSize = "14px"
        ),
        ".cm-content" -> js.Dynamic.literal(caretColor = "#000000"),
        ".cm-cursor" -> js.Dynamic.literal(borderLeftColor = "#000000"),
        ".cm-gutters" -> js.Dynamic.literal(
          backgroundColor = "#f3f3f3",
          color = "#808080",
          border = "none",
          borderRight = "1px solid #e0e0e0"
        ),
        ".cm-activeLineGutter" -> js.Dynamic.literal(backgroundColor = "#f0f0f0"),
        ".cm-activeLine" -> js.Dynamic.literal(backgroundColor = "#f0f0f0"),
        ".cm-selectionBackground, ::selection" ->
            js.Dynamic.literal(backgroundColor = "#add6ff"),
        ".cm-foldGutter" -> js.Dynamic.literal(color = "#808080"),
        ".cm-tooltip" -> js.Dynamic.literal(
          backgroundColor = "#f3f3f3",
          border = "1px solid #cccccc",
          color = "#000000",
          padding = "4px 8px",
          borderRadius = "4px"
        ),
        ".cm-tooltip-ndpc" -> js.Dynamic.literal(maxWidth = "400px", lineHeight = "1.5"),
        ".cm-lintRange-error" -> js.Dynamic.literal(
          backgroundImage =
              "url(\"data:image/svg+xml,%3Csvg%20xmlns%3D'http%3A//www.w3.org/2000/svg'%20width%3D'6'%20height%3D'3'%3E%3Cpath%20d%3D'M0%202%20L3%200%20L6%202'%20fill%3D'none'%20stroke%3D'%23c72e0f'%20stroke-width%3D'1.2'/%3E%3C/svg%3E\")",
          backgroundRepeat = "repeat-x",
          backgroundPosition = "bottom"
        )
      )
    )

    private val key = "ndpc-theme"

    val isDarkVar: Var[Boolean] = Var(dom.window.localStorage.getItem(key) != "light")

    /** Apply the correct body class on initial load. Call once from Main. */
    def initBodyClass(): Unit =
        if isDarkVar.now() then dom.document.body.classList.remove("theme-light")
        else dom.document.body.classList.add("theme-light")

    def initialExtension: Extension =
        compartment.of((if isDarkVar.now() then dark else light).asInstanceOf[Extension])

    def toggle(view: EditorView): Unit =
        val nowDark = isDarkVar.now()
        val newTheme = if nowDark then light else dark
        dom.window.localStorage.setItem(key, if nowDark then "light" else "dark")
        isDarkVar.set(!nowDark)
        // Flip body class so CSS variables switch
        if nowDark then dom.document.body.classList.add("theme-light")
        else dom.document.body.classList.remove("theme-light")
        view.asInstanceOf[js.Dynamic]
            .dispatch(
              js.Dynamic.literal(
                effects = compartment.reconfigure(newTheme.asInstanceOf[Extension])
              )
            )
