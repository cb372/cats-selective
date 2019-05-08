package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Cogen}
import org.typelevel.discipline.Laws

trait SelectiveTests[F[_]] extends Laws {
  def laws: SelectiveLaws[F]

  def selective[A: Arbitrary, B: Arbitrary, C: Arbitrary](
    implicit
    ArbFEitherA: Arbitrary[F[Either[A, A]]],
    ArbFEitherAB: Arbitrary[F[Either[A, B]]],
    ArbFEitherCAtoB: Arbitrary[F[Either[C, A => B]]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFCtoAtoB: Arbitrary[F[C => A => B]],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "selective",
      parent = None,
      "selective identity" -> forAll(laws.selectiveIdentity[A] _),
      "selective distributivity" -> forAll(laws.selectiveDistributivity[A, B] _),
      "selective associativity" -> forAll(laws.selectiveAssociativity[A, B, C] _)
    )
}

object SelectiveTests {

  def apply[F[_]: Selective]: SelectiveTests[F] =
    new SelectiveTests[F] { def laws: SelectiveLaws[F] = SelectiveLaws[F] }

  def monad[F[_]: Monad]: SelectiveTests[F] =
    new SelectiveTests[F] { def laws: SelectiveLaws[F] = SelectiveLaws[F](Selective.fromMonad) }

}
