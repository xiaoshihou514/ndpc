package ndpc.frontend.parsers

import parsley.errors.ErrorBuilder
import parsley.errors.DefaultErrorBuilder

abstract class ErrBuilder extends ErrorBuilder[EnrichedErr] {
    val Indent = " " * 2

    override def build(pos: Position, source: Source, lines: ErrorInfoLines): EnrichedErr =
        EnrichedErr(lines.mkString(Indent, "\n" + Indent, ""), source, pos._1, Some(pos._2))

    type Position = (Int, Int)
    type Source = Option[String]
    type UnexpectedLine = Option[String]
    type ExpectedLine = Option[String]
    type Message = String
    type LineInfo = Seq[String]
    type ExpectedItems = Option[String]
    type Messages = Seq[Message]
    type ErrorInfoLines = Seq[String]
    type Item = String
    type Raw = String
    type Named = String
    type EndOfInput = String
    override def pos(line: Int, col: Int): Position = (line, col)
    override def source(sourceName: Option[String]): Source = DefaultErrorBuilder.source(sourceName)
    override def vanillaError(
        unexpected: UnexpectedLine,
        expected: ExpectedLine,
        reasons: Messages,
        line: LineInfo
    ): ErrorInfoLines = DefaultErrorBuilder.vanillaError(unexpected, expected, reasons, line)
    override def specializedError(msgs: Messages, line: LineInfo): ErrorInfoLines =
        DefaultErrorBuilder.specializedError(msgs, line)
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems =
        DefaultErrorBuilder.disjunct(alts)
    override def combineMessages(alts: Seq[Message]): Messages =
        DefaultErrorBuilder.combineMessages(alts)
    override def unexpected(item: Option[Item]): UnexpectedLine =
        DefaultErrorBuilder.unexpected(item)
    override def expected(alts: ExpectedItems): ExpectedLine =
        DefaultErrorBuilder.expected(alts)
    override def reason(reason: String): Message =
        DefaultErrorBuilder.reason(reason)
    override def message(msg: String): Message =
        DefaultErrorBuilder.message(msg)
    override def lineInfo(
        line: String,
        linesBefore: Seq[String],
        linesAfter: Seq[String],
        lineNum: Int,
        errorPointsAt: Int,
        errorWidth: Int
    ): LineInfo = DefaultErrorBuilder.lineInfo(
      line,
      linesBefore,
      linesAfter,
      lineNum,
      errorPointsAt,
      errorWidth
    )
    override def raw(item: String): Raw = DefaultErrorBuilder.raw(item)
    override def named(item: String): Named = DefaultErrorBuilder.named(item)
    override val numLinesBefore: Int = DefaultErrorBuilder.NumLinesBefore
    override val numLinesAfter: Int = DefaultErrorBuilder.NumLinesAfter
    override val endOfInput: EndOfInput = DefaultErrorBuilder.EndOfInput
}

case class EnrichedErr(exp: String, file: Option[String], line: Int, column: Option[Int]):
    private def escape(s: String) = s
        .replace("\n", "\\n")
        .replace("\"", "\\\"")

    def location: String =
        column match
            case Some(col) => s"(line $line, column $col)"
            case None      => s"(line $line)"

    def toJson: String =
        column match
            case Some(col) =>
                s"""
                    |{
                    |  "file": "${file.get}",
                    |  "line": $line,
                    |  "column": $col,
                    |  "explanation": "${escape(exp)}"
                    |}
                    """.stripMargin
            case None =>
                s"""
                    |{
                    |  "file": "${file.get}",
                    |  "line": $line,
                    |  "explanation": "${escape(exp)}"
                    |}
                    """.stripMargin
