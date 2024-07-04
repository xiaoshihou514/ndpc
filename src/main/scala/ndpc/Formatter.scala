package ndpc
import scala.io.Source

def toContents(fname: String): String =
    Source.fromFile(fname).getLines().mkString("\n")

object Formatter {
    def format(input: List[String], apply: Boolean): Int = ???
}
