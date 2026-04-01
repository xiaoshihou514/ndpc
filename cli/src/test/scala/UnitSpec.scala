package ndpc

import org.scalatest.*
import flatspec.*
import matchers.*

abstract class UnitSpec
    extends AnyFlatSpec
    with should.Matchers
    with OptionValues
    with Inside
    with Inspectors
