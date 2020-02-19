package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Cogen}
import org.typelevel.discipline.Laws

trait RigidSelectiveTests[F[_]] extends Laws {
  def laws: RigidSelectiveLaws[F]

  // TODO - probably don't need all these implicit args.  Tidy up
  def selective[A: Arbitrary, B: Arbitrary, C: Arbitrary](
    implicit
    ArbFA: Arbitrary[F[A]],
    ArbFEitherA: Arbitrary[F[Either[A, A]]],
    ArbFEitherAB: Arbitrary[F[Either[A, B]]],
    ArbFEitherCAtoB: Arbitrary[F[Either[C, A => B]]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFCtoAtoB: Arbitrary[F[C => A => B]],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "rigid selective",
      parent = None,
      "rigid selective apply" -> forAll(laws.rigidSelectiveApply[A, B] _),
      "rigid selective interchange" -> forAll(laws.rigidSelectiveInterchange[A, B] _)
    )
}

object RigidSelectiveTests {

  def apply[F[_]: Selective]: RigidSelectiveTests[F] =
    new RigidSelectiveTests[F] { def laws: RigidSelectiveLaws[F] = RigidSelectiveLaws[F] }

  def monad[F[_]: Monad]: RigidSelectiveTests[F] =
    new RigidSelectiveTests[F] { def laws: RigidSelectiveLaws[F] = RigidSelectiveLaws[F](Selective.fromMonad) }

}
