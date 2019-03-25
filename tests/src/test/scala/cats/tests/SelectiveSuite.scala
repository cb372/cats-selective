package cats.tests

import cats._
import cats.instances.list._

class SelectiveSuite extends CatsSuite {
  implicit val testInstance: Selective[List] = Selective.fromMonad[List]

  // TODO write tests for combinators here

  test("example") {
    forAll { x: Int =>
      x should ===(x)
    }
  }

}
