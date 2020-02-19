package cats.laws

import cats.Selective
import cats.Selective.ops._

/**
  * A selective functor is _rigid_ if it satisfies `<*> = apS`.
  *
  * These laws are a consequence of that assumption
  */
trait RigidSelectiveLaws[F[_]] {
  implicit def F: Selective[F]

  /**
    * Selectives that satisfy this criteria are known as rigid selectives.
    * 
    * `applicative.ap === apS`
    */
  def rigidSelectiveApply[A, B](x: F[A], fn: F[A => B]): IsEq[F[B]] = {
    val lhs = F.apS(fn)(x)
    val rhs = F.applicative.ap(fn)(x)
    lhs <-> rhs
  }

  /**
    * A consequence of `applicative.ap === apS` and associativity, so don't strictly
    * need to test this law, but I'm writing it as a sanity check
    *
    * `x *> (y <*? z) === (x *> y) <*? z
    */
  def rigidSelectiveInterchange[A, B](x: F[A], y: F[Either[A, B]], z: F[A => B]): IsEq[F[B]] = {
    val lhs = F.applicative.*>(x)(F.select(y)(z))
    val rhs = F.select(F.applicative.*>(x)(y))(z)
    lhs <-> rhs
  }

}

object RigidSelectiveLaws {
  def apply[F[_]](implicit ev: Selective[F]): RigidSelectiveLaws[F] =
    new RigidSelectiveLaws[F] { def F: Selective[F] = ev }
}
