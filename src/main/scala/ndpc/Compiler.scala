package ndpc

import parsley.Result
import ndpc.Utils._
import ndpc.Checker.pfFromSource
import parsley.Failure
import parsley.Success
import os.RelPath
import scala.util.Try
import scala.io.Source
import ndpc.Parser.PfScope
import ndpc.Parser.Empty
import ndpc.Parser.Comment
import ndpc.Parser.Pf
import ndpc.expr.Formula.LFormula
import ndpc.expr.Rule.Rule

object Compiler {
    def compile(inputs: List[String], userCSS: Option[String]): Int = {
        val results = HTMLfromSource(inputs, userCSS)
        val errors = results.filter(_.isFailure).asInstanceOf[List[Failure[NdpcError]]]
        val successes =
            results
                .filter(_.isSuccess)
                .map(_.get)

        if !errors.isEmpty then printErrorHuman(errors)
        var errcount = errors.length

        for ((dest, formatted) <- successes) do {
            try {
                os.write.over(os.pwd / RelPath(dest), formatted)
            } catch {
                case e =>
                    errcount = errcount + 1
                    printerrln(s"Failed to write to $dest: $e")
            }
        }
        errcount
    }

    private def HTMLfromSource(
        inputs: List[String],
        css: Option[String]
    ): List[Result[NdpcError, (String, String)]] =
        pfFromSource(inputs).zip(inputs).map { (pf, dest) =>
            pf match
                case Success(pf)    => Success((chext(dest), compileFromString(pf, css)))
                case f @ Failure(_) => f
        }

    private def chext(orig: String): String =
        // just dirty regex
        orig.replaceAll("\\.[^.]*$", "") + ".html"

    private def compileFromString(
        pf: CheckedProof,
        cssPath: Option[String]
    ): String = {
        val css = cssPath match
            case None => defaultCSS
            case Some(path) =>
                Try(path).map(Source.fromFile(_).mkString) match
                    case scala.util.Failure(e) =>
                        printerrln(s"Failed to read from $path for custom CSS: $e")
                        printerrln("Using default css instead")
                        defaultCSS
                    case scala.util.Success(value) => value
        val (body, _) = toHTML(pf.main, 1)
        HTML5(css, body)
    }

    private def toHTML(s: PfScope, lineNr: Int): (String, Int) =
        var current = lineNr
        (
          s"""
    <div class="box"><ul>
    ${s.body.map {
                  _ match
                      case Pf(concl, rule, _) =>
                          val res = mkLine(concl, rule, current)
                          current = current + 1
                          res
                      case s @ PfScope(_) =>
                          val (res, newLineNr) = toHTML(s, lineNr)
                          current = newLineNr
                          "<li>" + res + "</li>"
                      case _ => ""
              }.mkString}
    </ul></div>
    """,
          current
        )

    private def mkLine(concl: LFormula, rule: Rule, lineNr: Int): String =
        s"""
        <li>
            <p>$lineNr</p>
            ${concl.toHTML}
            <div class="rule">${rule.toHTML}</div>
        </li>
    """

    private def HTML5(css: String, body: String) =
        s"""
        <!doctype html>
        <html lang="en">
          <head>
            <meta charset="UTF-8">
            <title>Proof</title>
            <style>
            $css
            </style>
            <body>
            $body
            </body>
        </html>
    """

    private val defaultCSS = s"""
      body {
        background-color: #1e1e2e;
        color: #f5e0dc;
        display: inline-block;
        margin: 0;
        padding: 20px;
      }

      .box {
        background-color: #313244;
        border-radius: 8px;
        padding: 15px;
        margin: 10px;
        box-shadow: 0 2px 10px rgba(0, 0, 0, 0.2);
      }

      .box > ul {
        list-style-type: none;
        padding: 0;
      }

      .box li {
        display: flex;
        justify-content: space-between;
        align-items: center;
        color: #cdd6f4;
        border-radius: 5px;
        font-size: 28px;
        transition:
          background-color 0.3s,
          box-shadow 0.3s;
      }

      .box li:hover {
        background-color: #585b70;
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.3);
      }

      .box p {
        display: flex;
        justify-content: center;
        align-items: center;
        width: 30px;
        height: 30px;
        border: 2px solid #f5c2e7;
        border-radius: 50%;
        background-color: #f9e2af;
        color: #313244;
        margin-right: 10px;
        font-size: 20px;
        margin-left: 5px;
      }

      .rule {
        background-color: #cba6f7;
        color: #1e1e2e;
        font-size: 20px;
        padding: 5px 10px;
        margin-left: 200px;
        margin-right: 5px;
        border-radius: 5px;
        font-weight: bold;
      }
    """
}
