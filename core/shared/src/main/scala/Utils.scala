package ndpc

import parsley.{Result, Success, Failure}
import scala.language.implicitConversions
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

    implicit def parsleyResultToIterable[Err, A](result: Result[Err, A]): IterableOnce[A] =
        result match
            case Success(x) => List(x)
            case _          => Nil
}
