package ndpc.backend

import ndpc.frontend.checker.pfFromSource
import ndpc.frontend.parser.Comment
import ndpc.frontend.parser.Empty
import ndpc.frontend.parser.Pf
import ndpc.frontend.parser.PfScope
import ndpc.utils._
import ndpc.frontend.expr.formula.LFormula
import ndpc.frontend.expr.rule.Rule
import ndpc.frontend.parsers.EnrichedErr

import scala.collection.mutable.StringBuilder
import scala.io.Source
import scala.util.Try
import parsley.{Result, Success, Failure}
import ndpc.frontend.CheckedProof

object codegen {
    def generate(inputs: Seq[String], userCSS: Option[String]): Int = {
        val results = HTMLfromSource(inputs, userCSS)
        val errors = results.collect { case f @ Failure(_) => f }
        val successes = results.flatten

        if !errors.isEmpty then printErrorHuman(errors)

        var code = errors.length
        for ((dest, result) <- successes) do {
            Try(os.write.over(dest, result)) match
                case _: scala.util.Failure[_] => code = code + 1
                case _                        =>
        }
        code
    }

    private def HTMLfromSource(
        inputs: Seq[String],
        css: Option[String]
    ): Seq[Result[NdpcError, (os.Path, String)]] =
        pfFromSource(inputs).zip(inputs).map { (pf, dest) =>
            pf match
                case Success(pf)    => Success((outputPath(dest), compileFromString(pf, css)))
                case f @ Failure(_) => f
        }

    private def outputPath(orig: String): os.Path =
        os.FilePath(
          orig.replaceAll("\\.[^.]*$", "") + ".html"
        ).resolveFrom(os.pwd)

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
        val (current, body) = s.body
            .foldLeft((lineNr, StringBuilder())) { (acc_, x) =>
                val (current, acc) = acc_
                x match
                    case Left(Pf(concl, rule, _)) =>
                        acc ++= mkLine(concl, rule, current)
                        (current + 1, acc)
                    case Right(s @ PfScope(_)) =>
                        val (res, newLineNr) = toHTML(s, current)
                        acc ++= s"<li>$res</li>"
                        (newLineNr, acc)
                    case _ => (current, acc)
            }
        (
          s"""
            <div class="box"><ul>
                $body
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
