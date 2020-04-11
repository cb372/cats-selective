package cats.tests

import cats.data._
import cats.laws.discipline._
import org.scalacheck.Arbitrary._
import cats.laws.discipline.arbitrary._
import cats.selected.instances.validated._

class ValidatedSuite extends CatsSuite {

  implicit val iso = SemigroupalTests.Isomorphisms.invariant[Validated[String, ?]]

  checkAll("Validated[String, Int]", SelectiveTests[Validated[String, ?]].selective[Int, Int, Int])

  // This fails, as expected, because Validated[E,?] is no a *rigid* selective functor
  // Useful for checking that the rigid laws fail when they are expected to!
  // checkAll("Validated[String, Int]", RigidSelectiveTests[Validated[String, ?]].selective[Int, Int])
}
