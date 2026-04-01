package ndpc.backend

import cats.effect.IO
import ndpc.CliRuntime
import ndpc.frontend.CheckedProof
import ndpc.frontend.expr.formula.*
import ndpc.frontend.expr.rule.*
import ndpc.frontend.parser.{Pf, PfScope, Line}

import scala.collection.mutable.StringBuilder

private def parenthesizeHTML = paren(_.asHTML)

// Extension functions for HTML representation
extension (f: LFormula)
    def asHTML: String = f match {
        case PredAp(p, args) =>
            if args == Nil then p
            else s"$p(${args.map(_.asHTML).mkString(", ")})"
        case Eq(left, right)  => s"${left.asHTML} = ${right.asHTML}"
        case Truth            => "&top;"
        case Falsity          => "&perp;"
        case Not(pf)          => s"&not;${parenthesizeHTML(f, pf)}"
        case And(left, right) => s"${parenthesizeHTML(f, left)} &and; ${parenthesizeHTML(f, right)}"
        case Or(left, right)  => s"${parenthesizeHTML(f, left)} &or; ${parenthesizeHTML(f, right)}"
        case Implies(left, right) =>
            s"${parenthesizeHTML(f, left)} &rarr; ${parenthesizeHTML(f, right)}"
        case Equiv(left, right) =>
            s"${parenthesizeHTML(f, left)} &LeftRightArrow; ${parenthesizeHTML(f, right)}"
        case Forall(x, body) => s"&forall; $x. (${body.asHTML})"
        case Exists(x, body) => s"&exist; $x. (${body.asHTML})"
    }

extension (r: Rule)
    def asHTML: String = r match {
        case AndIntro(left, right)         => s"&and;I($left, $right)"
        case ImpliesIntro(ass, res)        => s"&rarr;I($ass, $res)"
        case OrIntro(either)               => s"&or;I($either)"
        case NotIntro(orig, bottom)        => s"&not;I($orig, $bottom)"
        case DoubleNegIntro(orig)          => s"&not;&not;I($orig)"
        case FalsityIntro(orig, negated)   => s"&perp;I($orig, $negated)"
        case TruthIntro                    => "&top;I"
        case EquivIntro(leftImp, rightImp) => s"&LeftRightArrow;I($leftImp, $rightImp)"
        case ExistsIntro(orig)             => s"&exist;I($orig)"
        case ForallIntro(const, concl)     => s"&forall;I($const, $concl)"
        case AndElim(orig)                 => s"&and;E($orig)"
        case ImpliesElim(ass, imp)         => s"&rarr;E($ass, $imp)"
        case OrElim(or, leftAss, leftConcl, rightAss, rightConcl) =>
            s"&or;E($or, $leftAss, $leftConcl, $rightAss, $rightConcl)"
        case NotElim(negated, orig)         => s"&not;E($negated, $orig)"
        case DoubleNegElim(orig)            => s"&not;&not;E($orig)"
        case FalsityElim(bottom)            => s"&perp;E($bottom)"
        case EquivElim(equiv, either)       => s"&LeftRightArrow;E($equiv, $either)"
        case ExistsElim(exists, ass, concl) => s"&exist;E($exists, $ass, $concl)"
        case ForallElim(orig)               => s"&forall;E($orig)"
        case ForallImpElim(ass, imp)        => s"&forall;->E($ass, $imp)"
        case LEM                            => "LEM"
        case MT(imp, not)                   => s"MT($imp, $not)"
        case PC(orig, bottom)               => s"PC($orig, $bottom)"
        case Refl                           => "refl"
        case EqSub(orig, eq)                => s"=sub($orig, $eq)"
        case Sym(orig)                      => s"sym($orig)"
        case ForallIConst                   => "forall I const"
        case Given                          => "given"
        case Premise                        => "premise"
        case Ass                            => "ass"
        case Tick(orig)                     => s"&#10003;($orig)"
    }

object html extends Codegen[Option[os.Path]] {
    override val ext = "html"

    override def compile(
        pf: CheckedProof,
        cssPath: Option[os.Path],
        runtime: CliRuntime
    ): IO[String] =
        cssPath match
            case None => IO.pure(renderHtml(pf, defaultCSS))
            case Some(path) =>
                runtime.readPath(path).attempt.flatMap {
                    case Right(css) => IO.pure(renderHtml(pf, css))
                    case Left(e) =>
                        runtime.stderrln(s"Failed to read from $path for custom CSS: $e") *>
                            runtime.stderrln("Using default css instead") *>
                            IO.pure(renderHtml(pf, defaultCSS))
                }

    private def renderHtml(pf: CheckedProof, css: String): String =
        val (body, _) = toHTML(pf.main, 1)
        HTML5(css, body)

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
             ${concl.asHTML}
             <div class="rule">${rule.asHTML}</div>
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
