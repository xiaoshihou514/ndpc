package ndpc.expr

object Formula {
    case class Function(name: String, arity: Int)
    case class Predicate(name: String, arity: Int)
    // Definition 4.2 (term)
    // Fix a signature L.
    sealed trait LTerm {
        def getVars(): Set[String]
        def substitute(from: String, to: String): LTerm
    }
    // 1. Any constant in L is an L-term.
    // 2. Any variable is an L-term.
    // here we just consider variable since
    //  1. it's hard to differentiate the semantics during parsing
    //  2. it's not too useful for use to differentiate the two
    case class Variable(name: String) extends LTerm {
        override def toString(): String = name
        def getVars(): Set[String] = Set(name)
        def substitute(from: String, to: String): LTerm =
            if name == from then Variable(to)
            else this
    }
    // 3. If f is an n-ary function symbol in L, and t1...tn are L-terms, then f (t1...tn) is an L-term.
    case class FuncAp(f: Function, args: List[LTerm]) extends LTerm {
        override def toString(): String = s"${f.name}(${args.mkString(", ")})"
        def getVars(): Set[String] = args.map(_.getVars()).flatten.toSet
        def substitute(from: String, to: String): LTerm =
            this.copy(args = args.map(_.substitute(from, to)))
    }
    // 4. Nothing else is an L-term.

    // Definition 4.3 (formula)
    sealed trait LFormula {
        def getVars(): Set[String]
        def substitute(from: String, to: String): LFormula
    }
    // 1. If R is an n-ary predicate symbol in L, and t1...tn are
    // L-terms, then R(t1...tn) is an atomic L-formula.
    case class PredAp(p: Predicate, args: List[LTerm]) extends LFormula {
        override def toString(): String = s"${p.name}(${args.mkString(", ")})"
        def getVars(): Set[String] = args.map(_.getVars()).flatten.toSet
        def substitute(from: String, to: String) =
            this.copy(args = args.map(_.substitute(from, to)))
    }

    // 2. If t, t' are L-terms then t = t' is an atomic L-formula.
    case class Eq(left: LTerm, right: LTerm) extends LFormula {
        override def toString(): String = s"$left = $right"
        def getVars(): Set[String] = left.getVars() union right.getVars()
        def substitute(from: String, to: String) = Eq(
          left = left.substitute(from, to),
          right = right.substitute(from, to)
        )
    }

    // 3. ⊤ and ⊥ are atomic L-formulas.
    case class Truth() extends LFormula {
        override def toString(): String = "T"
        def getVars(): Set[String] = Set()
        def substitute(from: String, to: String) = Truth()
    }
    case class Falsity() extends LFormula {
        override def toString(): String = "F"
        def getVars(): Set[String] = Set()
        def substitute(from: String, to: String) = Falsity()
    }

    // 4. If 𝝓, φ are L-formulas then so are ¬𝝓, (𝝓 ∧ φ), (𝝓 ∨ φ), (𝝓 → φ), and (𝝓 ↔ φ).
    case class Not[A](pf: LFormula) extends LFormula {
        override def toString(): String = s"~($pf)"
        def getVars(): Set[String] = pf.getVars()
        def substitute(from: String, to: String) = Not(
          pf.substitute(from, to)
        )
    }

    case class And[A, B](left: LFormula, right: LFormula) extends LFormula {
        override def toString(): String = s"$left ^ $right"
        def getVars(): Set[String] = left.getVars() union right.getVars()
        def substitute(from: String, to: String) = And(
          left = left.substitute(from, to),
          right = right.substitute(from, to)
        )
    }

    case class Or[A, B](left: LFormula, right: LFormula) extends LFormula {
        override def toString(): String = s"$left / $right"
        def getVars(): Set[String] = left.getVars() union right.getVars()
        def substitute(from: String, to: String) = Or(
          left = left.substitute(from, to),
          right = right.substitute(from, to)
        )
    }

    case class Implies[A, B](left: LFormula, right: LFormula) extends LFormula {
        override def toString(): String = s"$left -> $right"
        def getVars(): Set[String] = left.getVars() union right.getVars()
        def substitute(from: String, to: String) = Implies(
          left = left.substitute(from, to),
          right = right.substitute(from, to)
        )
    }

    case class Equiv[A, B](left: LFormula, right: LFormula) extends LFormula {
        override def toString(): String = s"$left <-> $right"
        def getVars(): Set[String] = left.getVars() union right.getVars()
        def substitute(from: String, to: String) = Equiv(
          left = left.substitute(from, to),
          right = right.substitute(from, to)
        )
    }

    // 5. If 𝝓 is an L-formula and x a variable, then (∀x 𝝓) and (∃x 𝝓) are L-formulas.
    case class Forall[A](
        x: String,
        body: LFormula
    ) extends LFormula {
        override def toString(): String = s"forall $x ($body)"
        def getVars(): Set[String] = body.getVars() removedAll List(x)
        // PRE: from is not in vars (we only substitute _free_ variables!)
        def substitute(from: String, to: String) =
            this.copy(body = body.substitute(from, to))
    }

    case class Exists[A](
        x: String,
        body: LFormula
    ) extends LFormula {
        override def toString(): String = s"exists $x ($body)"
        def getVars(): Set[String] = body.getVars() removedAll List(x)
        // PRE: from is not in vars (we only substitute _free_ variables!)
        def substitute(from: String, to: String) =
            this.copy(body = body.substitute(from, to))
    }
}
