package ndpc.backend

import ndpc.frontend.CheckedProof

object lean extends codegen[Unit] {
    override protected val ext: String = "lean"

    override def compile(pf: CheckedProof, _opt: Unit): String = ???
}
