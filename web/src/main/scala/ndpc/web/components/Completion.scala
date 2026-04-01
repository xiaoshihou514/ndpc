package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.JSConverters.*
import ndpc.web.Docs

@js.native @JSImport("@codemirror/autocomplete", JSImport.Namespace)
private object CmAutoComplete extends js.Object

object Completion:
    private val ac = CmAutoComplete.asInstanceOf[js.Dynamic]

    private val keywordCompletions: js.Array[js.Dynamic] = js.Array(
      completion("forall", "∀  universal quantifier"),
      completion("exists", "∃  existential quantifier")
    )

    // All rule completions (inside brackets), built once from Docs
    private val ruleCompletions: js.Array[js.Dynamic] =
        Docs.docs.toArray
            .filterNot { (k, _) => k == "forall" || k == "exists" }
            .map { (label, doc) =>
                js.Dynamic.literal(label = label, `type` = "function", info = doc)
            }
            .toJSArray

    private def completion(label: String, detail: String): js.Dynamic =
        js.Dynamic.literal(label = label, `type` = "keyword", detail = detail)

    /** Returns true if the cursor at `pos` in `doc` is inside an open `[` bracket. */
    private def inBracket(docStr: String, pos: Int): Boolean =
        val prefix = docStr.substring(0, pos)
        prefix.contains("[") && !prefix.contains("]")

    private val ndpcCompletionSource: js.Function1[js.Dynamic, js.Any] =
        (ctx: js.Dynamic) =>
            val pos = ctx.pos.asInstanceOf[Int]
            val docStr = ctx.state.doc.toString().asInstanceOf[String]
            if inBracket(docStr, pos) then
                // Inside brackets: always offer rule completions.
                // Scan back to the last delimiter to find the token start.
                // CM6 will filter the list by what the user has typed so far.
                var from = pos
                while from > 0 && !" ,[()\n".contains(docStr.charAt(from - 1)) do from -= 1
                js.Dynamic.literal(
                  from = from,
                  options = ruleCompletions,
                  // Keep the popup open as more characters are typed
                  validFor = new js.RegExp("[^\\s,\\[\\]()]*")
                )
            else
                // Outside brackets: only complete word-based keywords (forall, exists).
                // Operators are single/two-char symbols that don't benefit from popup completion.
                val word = ctx.matchBefore(new js.RegExp("\\w+"))
                if (word == null && !ctx.explicit.asInstanceOf[Boolean]) null.asInstanceOf[js.Any]
                else
                    val from = if word == null then pos else word.from.asInstanceOf[Int]
                    js.Dynamic.literal(from = from, options = keywordCompletions)

    val extension: js.Any = ac.autocompletion(
      js.Dynamic.literal(
        `override` = js.Array(ndpcCompletionSource)
      )
    )
