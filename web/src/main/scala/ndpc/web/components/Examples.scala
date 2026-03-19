package ndpc.web.components

object Examples:
    val default: String =
        """|p ^ q [premise]
           |p [^E(1)]
           |q [^E(1)]
           |q ^ p [^I(3, 2)]""".stripMargin

    val all: List[(String, String)] = List(
        "∧ intro/elim" -> default,

        "Modus ponens chain" ->
            """|p [premise]
               |p -> q [premise]
               |q -> r [premise]
               |q [->E(1, 2)]
               |r [->E(4, 3)]""".stripMargin,

        "Double negation" ->
            """|p [premise]
               |~~p [~~I(1)]
               |p [~~E(2)]""".stripMargin,

        "Implication intro (sub-proof)" ->
            """|q [premise]
               |  p [ass]
               |  p ^ q [^I(2, 1)]
               |p -> p ^ q [->I(2, 3)]""".stripMargin,

        "Law of Excluded Middle" ->
            """|p / ~p [LEM]""".stripMargin,
    )
