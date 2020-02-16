package cats.tests

import cats._
import cats.instances.all._
import cats.free.FreeRigidSelective
import scala.io.StdIn
import cats.data.Writer
import cats.syntax.`package`.nonEmptyTraverse

class FreeRigidSelectiveSuite extends CatsSuite {

  import FreeRigidSelectiveSuite._
  import FreeRigidSelectiveSuite.interpreters._

  val S = implicitly[Selective[Teletype]]
  val A = S.applicative

  test("readLine should execute correctly") {
    val (written, result) = FreeRigidSelective.runSelect(mock("hello"))(readLine).run

    result shouldBe ("hello")
    written shouldBe empty
  }

  test("print should execute correctly") {
    val (written, result) = FreeRigidSelective.runSelect(mock())(print("Hello World")).run

    result shouldBe ()
    written shouldBe List("Hello World")
  }

  test("pingPong should print \"pong\" when pinged") {
    val pingPong = S.whenS(A.fmap(readLine)(_ == "ping"))(print("pong"))
    val (written, result) = FreeRigidSelective.runSelect(mock("ping"))(pingPong).run

    result shouldBe ()
    written shouldBe List("pong")
  }

  test("pingPong should print nothing when not pinged") {
    val pingPong = S.whenS(A.fmap(readLine)(_ == "ping"))(print("pong"))
    val (written, result) = FreeRigidSelective.runSelect(mock("not ping"))(pingPong).run

    result shouldBe ()
    written shouldBe empty
  }

}

object FreeRigidSelectiveSuite {

  // PingPong example taken from the paper [1]
  // [1] https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf
  sealed trait TeletypeOp[A]
  final case class Read[A](parse: String => A) extends TeletypeOp[A]
  final case class Write[A](output: String, a: A) extends TeletypeOp[A]

  object TeletypeOp {
    implicit def functor: Functor[TeletypeOp] = new Functor[TeletypeOp] {
      def map[A, B](fa: TeletypeOp[A])(f: A => B): TeletypeOp[B] = fa match {
        case Read(parse) => Read(s => f(parse(s)))
        case Write(s, a) => Write(s, f(a))
      }
    }
  }

  type Teletype[A] = FreeRigidSelective[TeletypeOp, A]

  def read[A](parse: String => A): Teletype[A] =
    FreeRigidSelective.liftSelect[TeletypeOp, A](Read(parse))

  def write[A](s: String, a: A): Teletype[A] =
    FreeRigidSelective.liftSelect[TeletypeOp, A](Write(s, a))

  def readLine: Teletype[String] = read[String](identity)
  def print(s: String): Teletype[Unit] = write[Unit](s, ())

  object interpreters {

    implicit val mockSelective: Selective[MockState] = Selective.fromMonad[MockState]

    type MockState[A] = Writer[List[String], A]
    def mock(input: String = ""): TeletypeOp ~> MockState = new (TeletypeOp ~> MockState) {
      def apply[A](fa: TeletypeOp[A]): MockState[A] = fa match {
        case Read(parse) => Writer(Nil, parse(input))
        case Write(s, a) => Writer(List(s), a)
      }
    }

  }

}
