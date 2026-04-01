package ndpc

sealed trait RunOpt
case class CheckOpt(val json: Boolean) extends RunOpt
case class FormatOpt(val apply: Boolean) extends RunOpt
case object LatexGen extends RunOpt
case object TypstGen extends RunOpt
case object LeanGen extends RunOpt
case class HtmlGen(val css: Option[os.Path]) extends RunOpt
