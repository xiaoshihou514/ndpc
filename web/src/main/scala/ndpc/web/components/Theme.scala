package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import typings.codemirrorState.mod.{Compartment, Extension}
import typings.codemirrorView.mod.EditorView

@js.native @JSImport("@codemirror/view", JSImport.Namespace)
private object CmViewTheme extends js.Object

object Theme:
    val compartment: Compartment = new Compartment()

    // Access EditorView.theme() via the raw namespace so we get the real JS class.
    private val EditorViewJS = CmViewTheme.asInstanceOf[js.Dynamic].EditorView

    val dark: js.Any = EditorViewJS.theme(
        js.Dynamic.literal(
            "&" -> js.Dynamic.literal(
                backgroundColor = "#1e1e1e",
                color           = "#d4d4d4",
                height          = "100%",
                fontFamily      = "'Cascadia Code', 'Fira Code', 'Menlo', monospace",
                fontSize        = "14px",
            ),
            ".cm-content"           -> js.Dynamic.literal(caretColor = "#d4d4d4"),
            ".cm-cursor"            -> js.Dynamic.literal(borderLeftColor = "#d4d4d4"),
            ".cm-gutters"           -> js.Dynamic.literal(
                backgroundColor = "#1e1e1e",
                color           = "#858585",
                border          = "none",
                borderRight     = "1px solid #333",
            ),
            ".cm-activeLineGutter"  -> js.Dynamic.literal(backgroundColor = "#282828"),
            ".cm-activeLine"        -> js.Dynamic.literal(backgroundColor = "#282828"),
            ".cm-selectionBackground, ::selection" ->
                js.Dynamic.literal(backgroundColor = "#264f78"),
            ".cm-foldGutter"        -> js.Dynamic.literal(color = "#858585"),
            ".cm-tooltip"           -> js.Dynamic.literal(
                backgroundColor = "#252526",
                border          = "1px solid #454545",
                color           = "#cccccc",
                padding         = "4px 8px",
                borderRadius    = "4px",
            ),
            ".cm-tooltip-ndpc"      -> js.Dynamic.literal(maxWidth = "400px", lineHeight = "1.5"),
            ".cm-lintRange-error"   -> js.Dynamic.literal(
                backgroundImage =
                    "url(\"data:image/svg+xml,%3Csvg%20xmlns%3D'http%3A//www.w3.org/2000/svg'%20width%3D'6'%20height%3D'3'%3E%3Cpath%20d%3D'M0%202%20L3%200%20L6%202'%20fill%3D'none'%20stroke%3D'%23f44747'%20stroke-width%3D'1.2'/%3E%3C/svg%3E\")",
                backgroundRepeat   = "repeat-x",
                backgroundPosition = "bottom",
            ),
        ),
        js.Dynamic.literal(dark = true)
    )

    val light: js.Any = EditorViewJS.theme(
        js.Dynamic.literal(
            "&" -> js.Dynamic.literal(
                backgroundColor = "#ffffff",
                color           = "#1e1e1e",
                height          = "100%",
                fontFamily      = "'Cascadia Code', 'Fira Code', 'Menlo', monospace",
                fontSize        = "14px",
            ),
            ".cm-content"           -> js.Dynamic.literal(caretColor = "#000000"),
            ".cm-cursor"            -> js.Dynamic.literal(borderLeftColor = "#000000"),
            ".cm-gutters"           -> js.Dynamic.literal(
                backgroundColor = "#f8f8f8",
                color           = "#999999",
                border          = "none",
                borderRight     = "1px solid #dddddd",
            ),
            ".cm-activeLineGutter"  -> js.Dynamic.literal(backgroundColor = "#f0f0f0"),
            ".cm-activeLine"        -> js.Dynamic.literal(backgroundColor = "#f0f0f0"),
            ".cm-selectionBackground, ::selection" ->
                js.Dynamic.literal(backgroundColor = "#add6ff"),
            ".cm-foldGutter"        -> js.Dynamic.literal(color = "#999999"),
            ".cm-tooltip"           -> js.Dynamic.literal(
                backgroundColor = "#f8f8f8",
                border          = "1px solid #cccccc",
                color           = "#1e1e1e",
                padding         = "4px 8px",
                borderRadius    = "4px",
            ),
            ".cm-tooltip-ndpc"      -> js.Dynamic.literal(maxWidth = "400px", lineHeight = "1.5"),
            ".cm-lintRange-error"   -> js.Dynamic.literal(
                backgroundImage =
                    "url(\"data:image/svg+xml,%3Csvg%20xmlns%3D'http%3A//www.w3.org/2000/svg'%20width%3D'6'%20height%3D'3'%3E%3Cpath%20d%3D'M0%202%20L3%200%20L6%202'%20fill%3D'none'%20stroke%3D'%23e51400'%20stroke-width%3D'1.2'/%3E%3C/svg%3E\")",
                backgroundRepeat   = "repeat-x",
                backgroundPosition = "bottom",
            ),
        )
    )

    private val key = "ndpc-theme"

    /** The initial extension value for the compartment, respecting localStorage. */
    def initialExtension: Extension =
        val isDark = dom.window.localStorage.getItem(key) != "light"
        compartment.of((if isDark then dark else light).asInstanceOf[Extension])

    /** Toggle between dark and light, persist choice, and reconfigure the view. */
    def toggle(view: EditorView, button: dom.html.Button): Unit =
        val currentIsDark = dom.window.localStorage.getItem(key) != "light"
        val newTheme      = if currentIsDark then light else dark
        val newLabel      = if currentIsDark then "☀ Light" else "☾ Dark"
        dom.window.localStorage.setItem(key, if currentIsDark then "light" else "dark")
        button.textContent = newLabel
        view.asInstanceOf[js.Dynamic].dispatch(
            js.Dynamic.literal(
                effects = compartment.reconfigure(newTheme.asInstanceOf[Extension])
            )
        )
