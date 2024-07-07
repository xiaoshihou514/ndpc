//> using scala 3.3.3
//> using platform native
//> using nativeVersion 0.4.17
//> using dep com.github.j-mie6::parsley::5.0.0-M6
//> using dep org.scalatest::scalatest::3.2.18

package ndpc

import org.scalatest._
import flatspec._
import matchers._

abstract class UnitSpec extends AnyFlatSpec with should.Matchers with OptionValues with Inside with Inspectors
