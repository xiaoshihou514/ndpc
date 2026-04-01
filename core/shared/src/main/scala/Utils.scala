package ndpc

import parsley.{Result, Success, Failure}
import ndpc.frontend.parsers.EnrichedErr

object utils {
    case class ParserException(reason: EnrichedErr) extends Exception
    object ParserException:
        def unapply(e: ParserException): Option[EnrichedErr] = Some(e.reason)
    case class CheckException(reason: EnrichedErr) extends Exception
    object CheckException:
        def unapply(e: CheckException): Option[EnrichedErr] = Some(e.reason)

    sealed trait NdpcError
    case class IOError(file: String, reason: String) extends NdpcError
    case class SyntaxError(reason: EnrichedErr) extends NdpcError
    case class SemanticsError(reason: EnrichedErr) extends NdpcError
}
