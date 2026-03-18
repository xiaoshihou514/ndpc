package ndpc.web.components

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.DynamicImplicits.given
import ndpc.web.Docs

@js.native @JSImport("@codemirror/language", JSImport.Namespace)
private object CmLanguage extends js.Object

@js.native @JSImport("@lezer/highlight", JSImport.Namespace)
private object LezerHighlight extends js.Object

object Highlight:
    private val lang  = CmLanguage.asInstanceOf[js.Dynamic]
    private val lh    = LezerHighlight.asInstanceOf[js.Dynamic]
    private val tags  = lh.tags

    // Rule token strings sorted by length descending so longer patterns (e.g. "forall->E")
    // are tried before shorter prefixes (e.g. "forall").
    private val ruleTokens: js.Array[String] =
        (Docs.docs.keySet - "forall I const" - "forall" - "exists")
            .toArray
            .sortBy(-_.length)
            .toJSArray

    private val ndpcLanguage: js.Any = lang.StreamLanguage.define(
        js.Dynamic.literal(
            startState = (() => js.Dynamic.literal(inBracket = false)): js.Function0[js.Dynamic],
            token = ((stream: js.Dynamic, state: js.Dynamic) =>
                tokenize(stream, state)
            ): js.Function2[js.Dynamic, js.Dynamic, js.Any]
        )
    )

    private def tokenize(stream: js.Dynamic, state: js.Dynamic): js.Any =
        if state.inBracket then tokenizeInBracket(stream, state)
        else tokenizeOutside(stream, state)

    private def tokenizeInBracket(stream: js.Dynamic, state: js.Dynamic): js.Any =
        // Closing bracket
        if stream.eat("]") then
            state.inBracket = false
            "punctuation": js.Any
        // Rule names (greedy — longer names matched first)
        else if ruleTokens.exists(rn => stream.`match`(rn)) then "typeName": js.Any
        // Numbers (line references)
        else if stream.`match`(new js.RegExp("^\\d+")) then "number": js.Any
        // Everything else inside brackets (commas, parens, spaces)
        else
            stream.next()
            null.asInstanceOf[js.Any]

    private def tokenizeOutside(stream: js.Dynamic, state: js.Dynamic): js.Any =
        // Line comment: -- to end of line
        if stream.`match`("--") then
            while !stream.eol() do stream.next()
            "lineComment": js.Any
        // Opening rule bracket
        else if stream.eat("[") then
            state.inBracket = true
            "punctuation": js.Any
        // Logical keywords (must match whole word — check next char is not alphanum)
        else if stream.`match`(new js.RegExp("^(forall|exists)(?![\\w])")) then
            "keyword": js.Any
        // Logical operators (longest first to avoid prefix clashes)
        else if
            stream.`match`("<->") || stream.`match`("->") ||
            stream.`match`("~~")  || stream.`match`("~")  ||
            stream.`match`("^")   || stream.`match`("/")
        then "operator": js.Any
        // Numbers
        else if stream.`match`(new js.RegExp("^\\d+")) then "number": js.Any
        else
            stream.next()
            null.asInstanceOf[js.Any]

    private val highlightStyle: js.Any =
        lang.syntaxHighlighting(
            lang.HighlightStyle.define(js.Array(
                js.Dynamic.literal(tag = tags.lineComment, color = "#608b4e", fontStyle = "italic"),
                js.Dynamic.literal(tag = tags.keyword,     color = "#569cd6", fontWeight = "bold"),
                js.Dynamic.literal(tag = tags.operator,    color = "#d4d4d4"),
                js.Dynamic.literal(tag = tags.number,      color = "#b5cea8"),
                js.Dynamic.literal(tag = tags.typeName,    color = "#4ec9b0"),
                js.Dynamic.literal(tag = tags.punctuation, color = "#808080"),
            ))
        )

    /** The combined extension: language support + highlight style. */
    val extension: js.Array[js.Any] = js.Array(ndpcLanguage, highlightStyle)
