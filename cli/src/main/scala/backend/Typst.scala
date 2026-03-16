package ndpc.backend

import cats.effect.IO
import ndpc.frontend.CheckedProof
import ndpc.frontend.expr.formula._
import ndpc.frontend.expr.rule._
import ndpc.frontend.parser.{Pf, PfScope, Line}

extension (f: LFormula)
    def asTypst: String = f match {
        case PredAp(p, args) =>
            if args == Nil then p.mkString(" ")
            else s"\"$p\"(${args.map(_.asTypst).mkString(", ")})"
        case Eq(left, right) => s"${left.asTypst} = ${right.asTypst}"
        case Truth           => "top"
        case Falsity         => "bot"
        case Not(pf)         => s"not ${paren(f, pf)}"
        case And(left, right) =>
            s"${paren(f, left)} and ${paren(f, right)}"
        case Or(left, right) =>
            s"${paren(f, left)} or ${paren(f, right)}"
        case Implies(left, right) =>
            s"${paren(f, left)} -> ${paren(f, right)}"
        case Equiv(left, right) =>
            s"${paren(f, left)} <-> ${paren(f, right)}"
        case Forall(x, body) => s"forall $x. ${body.asTypst}"
        case Exists(x, body) => s"exists $x. ${body.asTypst}"
    }

// Helper function for parenthesis handling in asTypst
private def paren(parent: LFormula, child: LFormula): String = {
    def precedence(lf: LFormula): Int = lf match {
        case PredAp(_, _)  => 7
        case Truth         => 7
        case Falsity       => 7
        case Not(_)        => 6
        case Eq(_, _)      => 5
        case And(_, _)     => 4
        case Or(_, _)      => 3
        case Equiv(_, _)   => 2
        case Implies(_, _) => 1
        case Forall(_, _)  => 0
        case Exists(_, _)  => 0
    }

    if precedence(parent) < precedence(child) then child.asTypst
    else s"(${child.asTypst})"
}

extension (r: Rule)
    def asTypst: String = r match {
        case AndIntro(left, right)         => s"andi($left, $right)"
        case ImpliesIntro(ass, res)        => s"impi($ass, $res)"
        case OrIntro(either)               => s"ori($either)"
        case NotIntro(orig, bottom)        => s"noti($orig, $bottom)"
        case DoubleNegIntro(orig)          => s"dni($orig)"
        case FalsityIntro(orig, negated)   => s"fi($orig, $negated)"
        case TruthIntro                    => "ti"
        case EquivIntro(leftImp, rightImp) => s"iffi($leftImp, $rightImp)"
        case ExistsIntro(orig)             => s"exi($orig)"
        case ForallIntro(const, concl)     => s"fai($const, $concl)"
        case AndElim(orig)                 => s"ande($orig)"
        case ImpliesElim(ass, imp)         => s"impe($ass, $imp)"
        case OrElim(or, leftAss, leftConcl, rightAss, rightConcl) =>
            s"ore($or, $leftAss, $leftConcl, $rightAss, $rightConcl)"
        case NotElim(negated, orig)         => s"note($negated, $orig)"
        case DoubleNegElim(orig)            => s"dne($orig)"
        case FalsityElim(bottom)            => s"fe($bottom)"
        case EquivElim(equiv, either)       => s"iffe($equiv, $either)"
        case ExistsElim(exists, ass, concl) => s"exe($exists, $ass, $concl)"
        case ForallElim(orig)               => s"fae($orig)"
        case ForallImpElim(ass, imp)        => s"faie($ass, $imp)"
        case LEM                            => "lem"
        case MT(imp, not)                   => s"mt($imp, $not)"
        case PC(orig, bottom)               => s"pc($orig, $bottom)"
        case Refl                           => "refl"
        case EqSub(orig, eq)                => s"eqsub($orig, $eq)"
        case Sym(orig)                      => s"symm($orig)"
        case ForallIConst                   => "fic"
        case Given                          => "given"
        case Premise                        => "premise"
        case Ass                            => "ass"
        case Tick(orig)                     => s"tick($orig)"
    }

object typst extends codegen[Unit] {
    override protected val ext: String = "typ"

    override def compile(pf: CheckedProof, _opt: Unit): IO[String] = IO.pure {
        val (orLeft, orRight) = findOrElims(pf.main)
        val (body, _) = toTypst(pf.main, 1, orLeft, orRight)
        typstDocument(body)
    }

    private def toTypst(
        s: PfScope,
        lineNr: Int,
        orLeft: Set[(Int, Int)],
        orRight: Set[(Int, Int)]
    ): (String, Int) = {
        val (current, body) = processScope(s, lineNr, orLeft, orRight)
        (body.toString, current)
    }

    private def processScope(
        s: PfScope,
        line: Int,
        orLeft: Set[(Int, Int)],
        orRight: Set[(Int, Int)]
    ): (Int, StringBuilder) = {
        s.body.foldLeft((line, StringBuilder())) { case ((ln, acc), x) =>
            x match
                case Left(Pf(concl, rule, _)) =>
                    acc ++= mkLine(concl, rule)
                    (ln + 1, acc)
                case Right(sc: PfScope) =>
                    val (newLn, res) = processScope(sc, ln, orLeft, orRight)
                    if orLeft(ln, newLn - 1) then acc ++= s"cases(pf($res),"
                    else if orRight(ln, newLn - 1) then acc ++= s"pf($res)),"
                    else {
                        acc ++= s"pfbox(\n"
                        acc ++= res.toString
                        acc ++= "),\n"
                    }
                    (newLn, acc)
                case _ => (ln, acc)
        }
    }

    private def mkLine(concl: LFormula, rule: Rule): String =
        s"  ($$${concl.asTypst}$$, ${rule.asTypst}),\n"

    private def typstDocument(body: String) =
        s"""#import "@preview/boxproof:0.1.0": *
            |// typst compile *.typ
            |// Alternatively, preview on https://typst.app
            |#start(pf(
            |$body
            |))""".stripMargin
}
