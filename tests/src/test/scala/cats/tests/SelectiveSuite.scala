package cats.tests

import cats._
import cats.instances.option._

class SelectiveSuite extends CatsSuite {
  implicit val testInstance: Selective[Option] = Selective.fromMonad[Option]

  test("ifS") {
    forAll { (x: Option[Boolean], t: Option[Int], e: Option[Int]) =>
      x match {
        case None =>
          testInstance.ifS(x)(t)(e) should ===(None)
        case Some(true) =>
          testInstance.ifS(x)(t)(e) should ===(t)
        case Some(false) =>
          testInstance.ifS(x)(t)(e) should ===(e)
      }
    }
  }

}
