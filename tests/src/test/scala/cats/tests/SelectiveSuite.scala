package cats.tests

import cats._

class SelectiveSuite extends CatsSuite {

  // TODO write tests for combinators here
  test("example") {
    forAll { x: Int =>
      x should === (x)
    }
  }

}
