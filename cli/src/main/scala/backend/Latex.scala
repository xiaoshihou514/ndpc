package ndpc.cli.backend

import cats.effect.IO
import ndpc.cli.CliRuntime
import ndpc.frontend.CheckedProof
import ndpc.frontend.expr.formula.*
import ndpc.frontend.expr.rule.*
import ndpc.frontend.parser.{Pf, PfScope, Line}

private def parenthesizeLatex = paren(_.asLatex)

extension (f: LFormula)
    def asLatex: String = f match {
        case PredAp(p, args) =>
            if args == Nil then p
            else s"$p(${args.map(_.asLatex).mkString(", ")})"
        case Eq(left, right) => s"${left.asLatex} = ${right.asLatex}"
        case Truth           => "\\top"
        case Falsity         => "\\bot"
        case Not(pf)         => s"\\lnot ${parenthesizeLatex(f, pf)}"
        case And(left, right) =>
            s"${parenthesizeLatex(f, left)} \\land ${parenthesizeLatex(f, right)}"
        case Or(left, right) =>
            s"${parenthesizeLatex(f, left)} \\lor ${parenthesizeLatex(f, right)}"
        case Implies(left, right) =>
            s"${parenthesizeLatex(f, left)} \\rightarrow ${parenthesizeLatex(f, right)}"
        case Equiv(left, right) =>
            s"${parenthesizeLatex(f, left)} \\leftrightarrow ${parenthesizeLatex(f, right)}"
        case Forall(x, body) => s"\\forall $x. (${body.asLatex})"
        case Exists(x, body) => s"\\exists $x. (${body.asLatex})"
    }

extension (r: Rule)
    def asLatex: String = r match {
        case AndIntro(left, right)         => s"\\andintro($left, $right)"
        case ImpliesIntro(ass, res)        => s"\\impliesintro($ass, $res)"
        case OrIntro(either)               => s"\\orintro($either)"
        case NotIntro(orig, bottom)        => s"\\notintro($orig, $bottom)"
        case DoubleNegIntro(orig)          => s"\\doublenegintro($orig)"
        case FalsityIntro(orig, negated)   => s"\\bottomintro($orig, $negated)"
        case TruthIntro                    => "\\topintro"
        case EquivIntro(leftImp, rightImp) => s"\\iffintro($leftImp, $rightImp)"
        case ExistsIntro(orig)             => s"\\existsintro($orig)"
        case ForallIntro(const, concl)     => s"\\forallintro($const, $concl)"
        case AndElim(orig)                 => s"\\andelim($orig)"
        case ImpliesElim(ass, imp)         => s"\\implieselim($ass, $imp)"
        case OrElim(or, leftAss, leftConcl, rightAss, rightConcl) =>
            s"\\orelim($or, $leftAss-$leftConcl, $rightAss-$rightConcl)"
        case NotElim(negated, orig)         => s"\\notelim($negated, $orig)"
        case DoubleNegElim(orig)            => s"\\doublenegelim($orig)"
        case FalsityElim(bottom)            => s"\\bottomelim($bottom)"
        case EquivElim(equiv, either)       => s"\\iffelim($equiv, $either)"
        case ExistsElim(exists, ass, concl) => s"\\existselim($exists, $ass-$concl)"
        case ForallElim(orig)               => s"\\forallelim($orig)"
        case ForallImpElim(ass, imp)        => s"\\forallelim($ass, $imp)"
        case LEM                            => "\\lem"
        case MT(imp, not)                   => s"MT($imp, $not)"
        case PC(orig, bottom)               => s"PC($orig, $bottom)"
        case Refl                           => "\\hbox{refl}"
        case EqSub(orig, eq)                => s"=\\hbox{sub}($orig, $eq)"
        case Sym(orig)                      => s"sym($orig)"
        case ForallIConst                   => "\\forallintroconst"
        case Given                          => "\\hbox{given}"
        case Premise                        => "\\hbox{premise}"
        case Ass                            => "\\asm"
        case Tick(orig)                     => s"\\tick{$orig}"
    }

object latex extends Codegen[Unit] {
    override protected val ext: String = "tex"

    override def compile(pf: CheckedProof, _opt: Unit, _runtime: CliRuntime): IO[String] = IO.pure {
        // println(pf)
        val (orLeft, orRight) = findOrElims(pf.main)
        val (_, body) = toLatex(pf.main, 1, orLeft, orRight)
        latexDocument(body)
    }

    private def toLatex(
        s: PfScope,
        line: Int,
        orLeft: Set[(Int, Int)],
        orRight: Set[(Int, Int)]
    ): (Int, String) = {
        val (newLine, body) = s.body
            .foldLeft((line, StringBuilder())) { case ((ln, acc), x) =>
                x match
                    case Left(Pf(concl, rule, _)) =>
                        acc ++= mkLine(concl, rule)
                        (ln + 1, acc)
                    case Right(sc: PfScope) =>
                        val (newLn, res) = toLatex(sc, ln, orLeft, orRight)
                        if orLeft(ln, newLn - 1) then acc ++= s"\\openAlt\n$res"
                        else if orRight(ln, newLn - 1) then acc ++= s"\\splitAlt\n$res\\closeAlt\n"
                        else acc ++= s"\\open\n$res\\close\n"
                        (newLn, acc)
                    case _ => (ln, acc)
            }
        (newLine, body.toString)
    }

    private def mkLine(concl: LFormula, rule: Rule): String =
        s"\\: ${concl.asLatex} \\= ${rule.asLatex} \\\\\n"

    private def latexDocument(body: String) =
        s"""\\documentclass{article}
            |\\usepackage{amsmath}
            |\\usepackage{amssymb}
            |\\usepackage{boxproof}
            |\\usepackage[a4paper, margin=1in]{geometry}
            |\\begin{document}
            |\\begin{proofbox}
            |${body.trim}
            |\\end{proofbox}
            |\\end{document}""".stripMargin
}
