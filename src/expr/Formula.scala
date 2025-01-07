package ndpc.expr

object Formula {
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

    // fake precedence for making toString easier
    private def precedence(lf: LFormula) = {
        lf match
            // always no paren
            case PredAp(_, _) => 7
            case Truth()      => 7
            case Falsity()    => 7
            // maybe paren
            case Not(_)        => 6
            case Eq(_, _)      => 5
            case And(_, _)     => 4
            case Or(_, _)      => 3
            case Equiv(_, _)   => 2
            case Implies(_, _) => 1
            // always paren
            case Forall(_, _) => 0
            case Exists(_, _) => 0
    }

    private def p(thiz: LFormula, child: LFormula) =
        if precedence(thiz) < precedence(child) then s"$child"
        else s"($child)"
    private def ph(thiz: LFormula, child: LFormula) =
        if precedence(thiz) < precedence(child) then s"${child.toHTML}"
        else s"(${child.toHTML})"

    // Definition 4.3 (formula)
    sealed trait LFormula {
        def getVars: Set[String]
        // TODO: make this lazy
        def substitutes(from: LFormula, to: LFormula): Set[LFormula]
        def toHTML: String
    }
    // 1. If R is an n-ary predicate symbol in L, and t1...tn are
    // L-terms, then R(t1...tn) is an atomic L-formula.
    // NOTE: 0-arity predAp -> variable
    //       predAp -> funcAp
    case class PredAp(p: String, args: List[LFormula]) extends LFormula {
        override def toString: String =
            if args == Nil then p
            else s"${p}(${args.mkString(", ")})"
        def toHTML: String = this.toString
        def getVars: Set[String] =
            args.map(_.getVars).flatten.toSet incl p
        def substitutes(from: LFormula, to: LFormula) =
            if this == from then Set(to, this)
            else
                args.map(_.substitutes(from, to)) match {
                    case Nil => Set(this)
                    case cs  => seqN(cs).map(PredAp(p, _)) incl this
                }
    }

    // 2. If t, t' are L-terms then t = t' is an atomic L-formula.
    case class Eq(left: LFormula, right: LFormula) extends LFormula {
        override def toString: String = s"$left = $right"
        def toHTML: String = this.toString
        def getVars: Set[String] = left.getVars union right.getVars
        def substitutes(from: LFormula, to: LFormula) =
            seq2(
              left = left.substitutes(from, to),
              right = right.substitutes(from, to),
              Eq.apply
            )
    }

    // 3. ⊤ and ⊥ are atomic L-formulas.
    case class Truth() extends LFormula {
        override def toString: String = "T"
        def toHTML: String = "&top;"
        def getVars: Set[String] = Set()
        def substitutes(from: LFormula, to: LFormula) = Set(Truth())
    }
    case class Falsity() extends LFormula {
        override def toString: String = "F"
        def toHTML: String = "&perp;"
        def getVars: Set[String] = Set()
        def substitutes(from: LFormula, to: LFormula) = Set(Falsity())
    }

    // 4. If 𝝓, φ are L-formulas then so are ¬𝝓, (𝝓 ∧ φ), (𝝓 ∨ φ), (𝝓 → φ), and (𝝓 ↔ φ).
    case class Not(pf: LFormula) extends LFormula {
        override def toString: String =
            s"~${p(this, pf)}"
        def toHTML: String = s"&not;${ph(this, pf)}"
        def getVars: Set[String] = pf.getVars
        def substitutes(from: LFormula, to: LFormula) =
            pf.substitutes(from, to).map(Not.apply)
    }

    case class And(left: LFormula, right: LFormula) extends LFormula {
        override def toString: String = s"${p(this, left)} ^ ${p(this, right)}"
        def toHTML: String = s"${ph(this, left)} &and; ${ph(this, right)}"
        def getVars: Set[String] = left.getVars union right.getVars
        def substitutes(from: LFormula, to: LFormula) =
            seq2(
              left = left.substitutes(from, to),
              right = right.substitutes(from, to),
              And.apply
            )
    }

    case class Or(left: LFormula, right: LFormula) extends LFormula {
        override def toString: String = s"${p(this, left)} / ${p(this, right)}"
        def toHTML: String = s"${ph(this, left)} &or; ${ph(this, right)}"
        def getVars: Set[String] = left.getVars union right.getVars
        def substitutes(from: LFormula, to: LFormula) =
            seq2(
              left = left.substitutes(from, to),
              right = right.substitutes(from, to),
              Or.apply
            )
    }

    case class Implies(left: LFormula, right: LFormula) extends LFormula {
        override def toString: String = s"${p(this, left)} -> ${p(this, right)}"
        def toHTML: String = s"${ph(this, left)} &rarr; ${ph(this, right)}"
        def getVars: Set[String] = left.getVars union right.getVars
        def substitutes(from: LFormula, to: LFormula) =
            seq2(
              left = left.substitutes(from, to),
              right = right.substitutes(from, to),
              Implies.apply
            )
    }

    case class Equiv(left: LFormula, right: LFormula) extends LFormula {
        override def toString: String = s"${p(this, left)} <-> ${p(this, right)}"
        def toHTML: String = s"${ph(this, left)} &LeftRightArrow; ${ph(this, right)}"
        def getVars: Set[String] = left.getVars union right.getVars
        def substitutes(from: LFormula, to: LFormula) =
            seq2(
              left = left.substitutes(from, to),
              right = right.substitutes(from, to),
              Equiv.apply
            )
    }

    // 5. If 𝝓 is an L-formula and x a variable, then (∀x 𝝓) and (∃x 𝝓) are L-formulas.
    case class Forall(
        x: String,
        body: LFormula
    ) extends LFormula {
        override def toString: String = s"forall $x. ($body)"
        def toHTML: String = s"&forall; $x. (${body.toHTML})"
        def getVars: Set[String] = body.getVars removedAll List(x)
        // PRE: from is not in vars (we only substitute _free_ variables!)
        def substitutes(from: LFormula, to: LFormula) =
            body.substitutes(from, to).map(Forall(x, _))
    }

    case class Exists(
        x: String,
        body: LFormula
    ) extends LFormula {
        override def toString: String = s"exists $x. ($body)"
        def toHTML: String = s"&exist; $x. (${body.toHTML})"
        def getVars: Set[String] = body.getVars removedAll List(x)
        // PRE: from is not in vars (we only substitute _free_ variables!)
        def substitutes(from: LFormula, to: LFormula) =
            body.substitutes(from, to).map(Exists(x, _))
    }
}
