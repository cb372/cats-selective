package cats.tests

import cats.data._
import cats.laws.discipline._
import org.scalacheck.Arbitrary._
import cats.laws.discipline.arbitrary._
import cats.selected.instances.validated._

class ValidatedSuite extends CatsSuite {

  implicit val iso = SemigroupalTests.Isomorphisms.invariant[Validated[String, ?]]

  checkAll("Validated[String, Int]", SelectiveTests[Validated[String, ?]].selective[Int, Int, Int])

}
