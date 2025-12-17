package ndpc.backend

import ndpc.frontend.CheckedProof

object typst extends codegen[Unit] {
    override protected val ext: String = "typ"

    override protected def compile(pf: CheckedProof, _opt: Unit): String = ???
}
