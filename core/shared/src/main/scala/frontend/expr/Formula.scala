package ndpc.frontend.expr

import parsley.generic.*

object formula {
    private def seqN(cs: List[Set[LFormula]]): Set[List[LFormula]] =
        cs match {
            case c :: Nil => c.map(_ :: Nil)
            case c :: cs =>
                c.map(f => seqN(cs).map(f :: _)).flatten
            case _ => ??? // unreachable
        }

    private def seq2[A](
        left: Set[LFormula],
        right: Set[LFormula],
        f: ((LFormula, LFormula) => A)
    ): Set[A] =
        for {
            l <- left
            r <- right
        } yield f(l, r)

    private def downgrade(syms: Set[Symbol]): Set[Symbol] = syms.map {
        case Predicate(name, arity) => Function(name, arity)
        case it                     => it
    }

    private def symbols_(f: LFormula): Set[Symbol] = f match
        case PredAp(name, Nil) => Set(Predicate(name, 0))
        case _                 => f.symbols

    sealed trait Symbol {
        val name: String
    }
    case class Var(val name: String) extends Symbol
    case class Function(val name: String, arity: Int) extends Symbol
    case class Predicate(val name: String, arity: Int) extends Symbol

    // Definition 4.3 (formula)
    sealed trait LFormula {
        def symbols: Set[Symbol]
        def names: Set[String] = symbols.map(_.name)
        def subterms: Set[LFormula]
        // TODO: make this lazy
        // WTH does that mean
        def substitutes(from: LFormula, to: LFormula): Set[LFormula]
    }
    // 1. If R is an n-ary predicate symbol in L, and t1...tn are
    // L-terms, then R(t1...tn) is an atomic L-formula.
    // NOTE: 0-arity predAp -> variable
    //       predAp -> funcAp
    case class PredAp(p: String, args: List[LFormula]) extends LFormula {
        def symbols =
            downgrade(args.map(_.symbols).flatten.toSet)
                .incl(if args.length > 0 then Predicate(p, args.length) else Var(p))
        def subterms = args.flatMap(_.subterms).toSet incl this
        def substitutes(from: LFormula, to: LFormula) =
            if this == from then Set(to, this)
            else
                args.map(_.substitutes(from, to)) match {
                    case Nil => Set(this)
                    case cs  => seqN(cs).map(PredAp(p, _)) incl this
                }
    }
    object PredAp extends ParserBridge2[String, List[LFormula], PredAp]

    // 2. If t, t' are L-terms then t = t' is an atomic L-formula.
    case class Eq(left: LFormula, right: LFormula) extends LFormula {
        def subterms = (left.subterms union right.subterms) incl this
        def symbols = downgrade(left.symbols union right.symbols)
        def substitutes(from: LFormula, to: LFormula) =
            seq2(
              left = left.substitutes(from, to),
              right = right.substitutes(from, to),
              Eq.apply
            )
    }
    object Eq extends ParserBridge2[LFormula, LFormula, Eq]

    // 3. ⊤ and ⊥ are atomic L-formulas.
    case object Truth extends LFormula {
        def symbols = Set.empty
        def subterms = Set(this)
        def substitutes(from: LFormula, to: LFormula) = Set(Truth)
    }
    case object Falsity extends LFormula {
        def symbols = Set.empty
        def subterms = Set(this)
        def substitutes(from: LFormula, to: LFormula) = Set(Falsity)
    }

    // 4. If 𝝓, φ are L-formulas then so are ¬𝝓, (𝝓 ∧ φ), (𝝓 ∨ φ), (𝝓 → φ), and (𝝓 ↔ φ).
    case class Not(pf: LFormula) extends LFormula {
        def symbols = symbols_(pf)
        def subterms = pf.subterms incl this
        def substitutes(from: LFormula, to: LFormula) =
            pf.substitutes(from, to).map(Not.apply)
    }
    object Not extends ParserBridge1[LFormula, Not]

    case class And(left: LFormula, right: LFormula) extends LFormula {
        def symbols = symbols_(left) union symbols_(right)
        def subterms = (left.subterms union right.subterms) incl this
        def substitutes(from: LFormula, to: LFormula) =
            seq2(
              left = left.substitutes(from, to),
              right = right.substitutes(from, to),
              And.apply
            )
    }
    object And extends ParserBridge2[LFormula, LFormula, And]

    case class Or(left: LFormula, right: LFormula) extends LFormula {
        def symbols = symbols_(left) union symbols_(right)
        def subterms = (left.subterms union right.subterms) incl this
        def substitutes(from: LFormula, to: LFormula) =
            seq2(
              left = left.substitutes(from, to),
              right = right.substitutes(from, to),
              Or.apply
            )
    }
    object Or extends ParserBridge2[LFormula, LFormula, Or]

    case class Implies(left: LFormula, right: LFormula) extends LFormula {
        def symbols = symbols_(left) union symbols_(right)
        def subterms = (left.subterms union right.subterms) incl this
        def substitutes(from: LFormula, to: LFormula) =
            seq2(
              left = left.substitutes(from, to),
              right = right.substitutes(from, to),
              Implies.apply
            )
    }
    object Implies extends ParserBridge2[LFormula, LFormula, Implies]

    case class Equiv(left: LFormula, right: LFormula) extends LFormula {
        def symbols = symbols_(left) union symbols_(right)
        def subterms = (left.subterms union right.subterms) incl this
        def substitutes(from: LFormula, to: LFormula) =
            seq2(
              left = left.substitutes(from, to),
              right = right.substitutes(from, to),
              Equiv.apply
            )
    }
    object Equiv extends ParserBridge2[LFormula, LFormula, Equiv]

    // 5. If 𝝓 is an L-formula and x a variable, then (∀x 𝝓) and (∃x 𝝓) are L-formulas.
    case class Forall(
        x: String,
        body: LFormula
    ) extends LFormula {
        def symbols = symbols_(body) excl Var(x)
        def subterms = body.subterms incl this
        // PRE: from is not in vars (we only substitute _free_ variables!)
        def substitutes(from: LFormula, to: LFormula) =
            body.substitutes(from, to).map(Forall(x, _))
    }

    case class Exists(
        x: String,
        body: LFormula
    ) extends LFormula {
        def symbols = symbols_(body) excl Var(x)
        def subterms = body.subterms incl this
        // PRE: from is not in vars (we only substitute _free_ variables!)
        def substitutes(from: LFormula, to: LFormula) =
            body.substitutes(from, to).map(Exists(x, _))
    }
}
