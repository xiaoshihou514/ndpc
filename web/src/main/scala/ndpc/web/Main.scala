package ndpc.web

import org.scalajs.dom
import org.scalajs.dom.html
import ndpc.web.components.*

object Main:
    def main(args: Array[String]): Unit =
        val root = dom.document.getElementById("app")

        // ---- Toolbar ----
        val toolbar = dom.document.createElement("div").asInstanceOf[dom.html.Div]
        toolbar.className = "toolbar"

        val title = dom.document.createElement("span").asInstanceOf[dom.html.Span]
        title.className   = "toolbar-title"
        title.textContent = "ndpc"
        toolbar.appendChild(title)

        // ---- Editor container ----
        val editorContainer = dom.document.createElement("div").asInstanceOf[dom.html.Div]
        editorContainer.className = "editor-container"

        root.appendChild(toolbar)
        root.appendChild(editorContainer)

        // Create the CodeMirror editor
        val (view, statusSpan) = Editor.create(editorContainer)

        // ---- Status indicator (placed right after title) ----
        toolbar.insertBefore(statusSpan, toolbar.firstChild.nextSibling)

        // ---- Theme toggle ----
        val isDark    = dom.window.localStorage.getItem("ndpc-theme") != "light"
        val themeBtn  = dom.document.createElement("button").asInstanceOf[dom.html.Button]
        themeBtn.className   = "toolbar-btn"
        themeBtn.textContent = if isDark then "☀ Light" else "☾ Dark"
        themeBtn.addEventListener("click", (_: dom.Event) => Theme.toggle(view, themeBtn))
        toolbar.appendChild(themeBtn)

        // ---- Examples dropdown ----
        Examples.buildSelect(view, toolbar)
