package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import typings.codemirrorView.mod.EditorView

object Examples:
    val default: String =
        """|p ^ q [premise]
           |p [^E(1)]
           |q [^E(1)]
           |q ^ p [^I(3, 2)]""".stripMargin

    private val all: List[(String, String)] = List(
      "∧ intro/elim" -> default,
      "Modus ponens chain" ->
          """|p [premise]
               |p -> q [premise]
               |q -> r [premise]
               |q [->E(1, 2)]
               |r [->E(4, 3)]""".stripMargin,
      "Double negation" ->
          """|p [premise]
               |~~p [~~I(1)]
               |p [~~E(2)]""".stripMargin,
      "Implication intro (sub-proof)" ->
          """|q [premise]
               |  p [ass]
               |  p ^ q [^I(2, 1)]
               |p -> p ^ q [->I(2, 3)]""".stripMargin,
      "Law of Excluded Middle" ->
          """|p / ~p [LEM]""".stripMargin
    )

    /** Appends a <select> dropdown to `parent` that loads examples into the editor. */
    def buildSelect(view: EditorView, parent: dom.Element): Unit =
        val select = dom.document.createElement("select").asInstanceOf[dom.html.Select]
        select.className = "toolbar-select"

        val placeholder = dom.document.createElement("option").asInstanceOf[dom.html.Option]
        placeholder.text = "Load example…"
        placeholder.disabled = true
        placeholder.selected = true
        select.appendChild(placeholder)

        all.foreach { (name, text) =>
            val opt = dom.document.createElement("option").asInstanceOf[dom.html.Option]
            opt.text = name
            opt.value = text
            select.appendChild(opt)
        }

        select.addEventListener(
          "change",
          (_: dom.Event) =>
              val text = select.value
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
              select.selectedIndex = 0
        )

        parent.appendChild(select)
