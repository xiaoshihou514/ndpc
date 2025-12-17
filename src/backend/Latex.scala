package ndpc.backend

import ndpc.frontend.CheckedProof

object latex extends codegen[Unit] {
    override protected val ext: String = "tex"

    override protected def compile(pf: CheckedProof, _opt: Unit): String = ???
}
