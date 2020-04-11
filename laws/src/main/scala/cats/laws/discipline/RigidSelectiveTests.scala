package cats
package laws
package discipline

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws

trait RigidSelectiveTests[F[_]] extends Laws {
  def laws: RigidSelectiveLaws[F]

  def selective[A: Arbitrary, B: Arbitrary](
    implicit
    ArbFA: Arbitrary[F[A]],
    ArbFEitherAB: Arbitrary[F[Either[A, B]]],
    ArbFAtoB: Arbitrary[F[A => B]],
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
