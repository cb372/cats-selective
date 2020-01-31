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

  test("whenS") {
    forAll { (x: Option[Boolean], t: Option[Unit]) =>
      x match {
        case None =>
          testInstance.whenS(x)(t) should ===(None)
        case Some(true) =>
          testInstance.whenS(x)(t) should ===(t)
        case Some(false) =>
          testInstance.whenS(x)(t) should ===(Some(()))
      }
    }
  }

  test("bindBool") {
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

  test("fromMaybeS") {
    forAll { (x: Option[Int], t: Option[Option[Int]]) =>
      (x, t) match {
        case (None, Some(Some(_))) =>
          testInstance.fromMaybeS(x)(t) should ===(t.flatten)
        case (None, Some(None)) =>
          testInstance.fromMaybeS(x)(t) should ===(None)
        case (None, None) =>
          testInstance.fromMaybeS(x)(t) should ===(None)
        case (Some(j), None) =>
          testInstance.fromMaybeS(x)(t) should ===(None)
        case (Some(j), Some(None)) =>
          testInstance.fromMaybeS(x)(t) should ===(x)
        case (Some(j), Some(Some(_))) =>
          testInstance.fromMaybeS(x)(t) should ===(t.flatten)
      }
    }
  }

  test("orS") {
    forAll { (x: Option[Boolean], t: Option[Boolean]) =>
      (x, t) match {
        case (None, _) =>
          testInstance.orS(x)(t) should ===(None)
        case (Some(true), _) =>
          testInstance.orS(x)(t) should ===(Option(true))
        case (Some(false), None) =>
          testInstance.orS(x)(t) should ===(None)
        case (Some(false), Some(true)) =>
          testInstance.orS(x)(t) should ===(Option(true))
        case (Some(false), Some(false)) =>
          testInstance.orS(x)(t) should ===(Option(false))
      }
    }
  }

  test("andS") {
    forAll { (x: Option[Boolean], t: Option[Boolean]) =>
      (x, t) match {
        case (None, _) =>
          testInstance.orS(x)(t) should ===(None)
        case (Some(false), None) =>
          testInstance.orS(x)(t) should ===(None)
        case (Some(false), Some(true)) =>
          testInstance.orS(x)(t) should ===(Option(true))
        case (Some(false), Some(false)) =>
          testInstance.orS(x)(t) should ===(Option(false))
        case (Some(true), None) =>
          testInstance.orS(x)(t) should ===(Option(true))
        case (Some(true), Some(true)) =>
          testInstance.orS(x)(t) should ===(Option(true))
        case (Some(true), Some(false)) =>
          testInstance.orS(x)(t) should ===(Option(true))
      }
    }
  }

  test("anyS") {
    forAll { (x: List[Boolean]) =>
      if (x.isEmpty)
        testInstance.anyS(Some.apply[Boolean])(x).value should ===(testInstance.pure(false))
      else {
        testInstance.anyS(Some.apply[Boolean])(x).value should ===(testInstance.pure(x.exists(identity)))
        testInstance.anyS((_: Boolean) => None)(x).value should ===(None)
      }
    }
  }

  test("allS") {
    forAll { (x: List[Boolean]) =>
      if (x.isEmpty)
        testInstance.allS(Some.apply[Boolean])(x).value should ===(testInstance.pure(true))
      else {
        testInstance.allS(Some.apply[Boolean])(x).value should ===(testInstance.pure(x.forall(identity)))
        testInstance.allS((_: Boolean) => None)(x).value should ===(None)
      }
    }
  }
}
