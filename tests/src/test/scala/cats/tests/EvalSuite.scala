package cats.tests

import cats.data._
import cats.laws.discipline._
import org.scalacheck.Arbitrary._
import cats.laws.discipline.arbitrary._
import cats.Eval
import cats.Selective

// TODO: abstract this to a general Monad because all monadic selective functors are rigid

class EvalSuite extends CatsSuite {

  implicit val selective = Selective.fromMonad[Eval]

  checkAll("Eval[Int]", SelectiveTests[Eval].selective[Int, Int, Int])
  checkAll("Eval[Int]", RigidSelectiveTests[Eval].selective[Int, Int, Int])
}
