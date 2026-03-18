package ndpc.web

import ndpc.frontend.{Checker as CoreChecker}
import ndpc.utils.*
import parsley.{Success, Failure}

final case class CheckError(line: Int, col: Int, message: String, kind: String)

object Checker:
    def check(text: String): List[CheckError] =
        CoreChecker.checkedFromString(text) match
            case Success(_) => Nil
            case Failure(SyntaxError(err)) =>
                List(CheckError(err.line, err.column.getOrElse(1), err.exp, "syntax"))
            case Failure(SemanticsError(err)) =>
                List(CheckError(err.line, err.column.getOrElse(1), err.exp, "semantics"))
            case Failure(IOError(_, reason)) =>
                List(CheckError(1, 1, reason, "io"))
