package ndpc.expr

object Formula {
    // Definition 4.3 (formula)
    sealed trait LFormula {
        def getVars(): Set[String]
        def substitute(from: LFormula, to: LFormula): LFormula
    }
    // 1. If R is an n-ary predicate symbol in L, and t1...tn are
    // L-terms, then R(t1...tn) is an atomic L-formula.
    // NOTE: 0-arity predAp -> variable
    //       predAp -> funcAp
    case class PredAp(p: String, args: List[LFormula]) extends LFormula {
        override def toString(): String =
            if args == Nil then p
            else s"${p}(${args.mkString(", ")})"
        def getVars(): Set[String] =
            args.map(_.getVars()).flatten.toSet incl p
        def substitute(from: LFormula, to: LFormula) =
            println(s"from: $from, this: $this, to: $to")
            val ret =
                if this == from then to
                else this.copy(args = args.map(_.substitute(from, to)))
            println(ret)
            ret
    }

    // 2. If t, t' are L-terms then t = t' is an atomic L-formula.
    case class Eq(left: LFormula, right: LFormula) extends LFormula {
        override def toString(): String = s"$left = $right"
        def getVars(): Set[String] = left.getVars() union right.getVars()
        def substitute(from: LFormula, to: LFormula) = Eq(
          left = left.substitute(from, to),
          right = right.substitute(from, to)
        )
    }

    // 3. ⊤ and ⊥ are atomic L-formulas.
    case class Truth() extends LFormula {
        override def toString(): String = "T"
        def getVars(): Set[String] = Set()
        def substitute(from: LFormula, to: LFormula) = Truth()
    }
    case class Falsity() extends LFormula {
        override def toString(): String = "F"
        def getVars(): Set[String] = Set()
        def substitute(from: LFormula, to: LFormula) = Falsity()
    }

    // 4. If 𝝓, φ are L-formulas then so are ¬𝝓, (𝝓 ∧ φ), (𝝓 ∨ φ), (𝝓 → φ), and (𝝓 ↔ φ).
    case class Not(pf: LFormula) extends LFormula {
        override def toString(): String = s"~($pf)"
        def getVars(): Set[String] = pf.getVars()
        def substitute(from: LFormula, to: LFormula) = Not(
          pf.substitute(from, to)
        )
    }

    case class And(left: LFormula, right: LFormula) extends LFormula {
        override def toString(): String = s"$left ^ $right"
        def getVars(): Set[String] = left.getVars() union right.getVars()
        def substitute(from: LFormula, to: LFormula) = And(
          left = left.substitute(from, to),
          right = right.substitute(from, to)
        )
    }

    case class Or(left: LFormula, right: LFormula) extends LFormula {
        override def toString(): String = s"$left / $right"
        def getVars(): Set[String] = left.getVars() union right.getVars()
        def substitute(from: LFormula, to: LFormula) = Or(
          left = left.substitute(from, to),
          right = right.substitute(from, to)
        )
    }

    case class Implies(left: LFormula, right: LFormula) extends LFormula {
        override def toString(): String = s"$left -> $right"
        def getVars(): Set[String] = left.getVars() union right.getVars()
        def substitute(from: LFormula, to: LFormula) = Implies(
          left = left.substitute(from, to),
          right = right.substitute(from, to)
        )
    }

    case class Equiv(left: LFormula, right: LFormula) extends LFormula {
        override def toString(): String = s"$left <-> $right"
        def getVars(): Set[String] = left.getVars() union right.getVars()
        def substitute(from: LFormula, to: LFormula) = Equiv(
          left = left.substitute(from, to),
          right = right.substitute(from, to)
        )
    }

    // 5. If 𝝓 is an L-formula and x a variable, then (∀x 𝝓) and (∃x 𝝓) are L-formulas.
    case class Forall(
        x: String,
        body: LFormula
    ) extends LFormula {
        override def toString(): String = s"forall $x ($body)"
        def getVars(): Set[String] = body.getVars() removedAll List(x)
        // PRE: from is not in vars (we only substitute _free_ variables!)
        def substitute(from: LFormula, to: LFormula) =
            this.copy(body = body.substitute(from, to))
    }

    case class Exists(
        x: String,
        body: LFormula
    ) extends LFormula {
        override def toString(): String = s"exists $x ($body)"
        def getVars(): Set[String] = body.getVars() removedAll List(x)
        // PRE: from is not in vars (we only substitute _free_ variables!)
        def substitute(from: LFormula, to: LFormula) =
            this.copy(body = body.substitute(from, to))
    }
}
