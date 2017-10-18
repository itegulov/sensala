package sensala

import com.typesafe.scalalogging.StrictLogging
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

class SensalaSpec
    extends FlatSpec
    with Matchers
    with Inspectors
    with Inside
    with OptionValues
    with EitherValues
    with TryValues
    with ScalaFutures
    with StrictLogging {
}