package ndpc.web

import com.raquo.laminar.api.L.*
import ndpc.frontend.checkerCore

import org.scalajs.dom

object Main {
    def main(args: Array[String]): Unit =
        val initialProof = Var("""x ^ y [premise]
x [^E(1)]""")

        val app = div(
          h1("ndpc web scaffold"),
          p("This Scala.js/Laminar app already links against the shared core proof logic."),
          textArea(
            rows := 8,
            cols := 60,
            value <-- initialProof.signal,
            onInput.mapToValue --> initialProof.writer
          ),
          pre(child.text <-- initialProof.signal.map(renderStatus))
        )

        renderOnDomContentLoaded(dom.document.getElementById("app"), app)

    private def renderStatus(input: String): String =
        checkerCore.checkedFromString(input) match
            case parsley.Success(_) => "Proof parses and checks in shared core."
            case parsley.Failure(error) =>
                error match
                    case ndpc.utils.SyntaxError(reason)    => s"Syntax error: ${reason.location}"
                    case ndpc.utils.SemanticsError(reason) => s"Semantics error: ${reason.location}"
                    case ndpc.utils.IOError(_, reason)     => s"I/O error: $reason"
}
