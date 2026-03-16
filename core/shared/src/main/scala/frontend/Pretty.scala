package ndpc.frontend

import ndpc.frontend.expr.formula.*
import ndpc.frontend.expr.rule.*
import ndpc.utils.*
import ndpc.frontend.parser.PfScope

case class CheckedProof(main: PfScope)

object pretty:
    extension (f: LFormula)
        def pretty: String = f match {
            case PredAp(p, args) =>
                if args == Nil then p
                else s"$p(${args.map(_.pretty).mkString(", ")})"
            case Eq(left, right) => s"${left.pretty} = ${right.pretty}"
            case Truth           => "T"
            case Falsity         => "F"
            case Not(pf)         => s"~${parenthesizeString(f, pf)}"
            case And(left, right) =>
                s"${parenthesizeString(f, left)} ^ ${parenthesizeString(f, right)}"
            case Or(left, right) =>
                s"${parenthesizeString(f, left)} / ${parenthesizeString(f, right)}"
            case Implies(left, right) =>
                s"${parenthesizeString(f, left)} -> ${parenthesizeString(f, right)}"
            case Equiv(left, right) =>
                s"${parenthesizeString(f, left)} <-> ${parenthesizeString(f, right)}"
            case Forall(x, body) => s"forall $x. (${body.pretty})"
            case Exists(x, body) => s"exists $x. (${body.pretty})"
        }

    extension (r: Rule)
        def pretty: String = r match {
            case AndIntro(left, right)         => s"^I($left, $right)"
            case ImpliesIntro(ass, res)        => s"->I($ass, $res)"
            case OrIntro(either)               => s"/I($either)"
            case NotIntro(orig, bottom)        => s"~I($orig, $bottom)"
            case DoubleNegIntro(orig)          => s"~~I($orig)"
            case FalsityIntro(orig, negated)   => s"FI($orig, $negated)"
            case TruthIntro                    => "TI"
            case EquivIntro(leftImp, rightImp) => s"<->I($leftImp, $rightImp)"
            case ExistsIntro(orig)             => s"existsI($orig)"
            case ForallIntro(const, concl)     => s"forallI($const, $concl)"
            case AndElim(orig)                 => s"^E($orig)"
            case ImpliesElim(ass, imp)         => s"->E($ass, $imp)"
            case OrElim(or, leftAss, leftConcl, rightAss, rightConcl) =>
                s"/E($or, $leftAss, $leftConcl, $rightAss, $rightConcl)"
            case NotElim(negated, orig)         => s"~E($negated, $orig)"
            case DoubleNegElim(orig)            => s"~~E($orig)"
            case FalsityElim(bottom)            => s"FE($bottom)"
            case EquivElim(equiv, either)       => s"<->E($equiv, $either)"
            case ExistsElim(exists, ass, concl) => s"existsE($exists, $ass, $concl)"
            case ForallElim(orig)               => s"forallE($orig)"
            case ForallImpElim(ass, imp)        => s"forall->E($ass, $imp)"
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
            case Tick(orig)                     => s"tick($orig)"
        }

    private def parenthesizeString(parent: LFormula, child: LFormula): String = {
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

        if precedence(parent) < precedence(child) then child.pretty
        else s"(${child.pretty})"
    }
