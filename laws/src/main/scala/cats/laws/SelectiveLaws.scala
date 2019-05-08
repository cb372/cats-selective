package cats.laws

import cats.Selective
import cats.Selective.ops._
import cats.syntax.either._

/**
 * Laws that must be obeyed by any `Selective`.
 */
trait SelectiveLaws[F[_]] {
  implicit def F: Selective[F]

  private def either[A, B, C](f: A => C, g: B => C)(x: Either[A, B]): C = x.bimap(f, g).merge

  def selectiveIdentity[A](x: F[Either[A, A]]): IsEq[F[A]] = {
    val lhs = x.select(F.pure(Predef.identity))
    val rhs = F.map(x)(either(Predef.identity, Predef.identity))
    lhs <-> rhs
  }

  def selectiveDistributivity[A, B](x: Either[A, B], y: F[A => B], z: F[A => B]): IsEq[F[B]] = {
    val lhs = F.pure(x).select(F.applicative.*>(y)(z))
    val rhs = F.applicative.*>(F.select(F.pure(x))(y))(F.select(F.pure(x))(z))
    lhs <-> rhs
  }

  def selectiveAssociativity[A, B, C](x: F[Either[A, B]], y: F[Either[C, A => B]], z: F[C => A => B]): IsEq[F[B]] = {

    val lhs: F[B] = x select (y select z)

    val p: F[Either[A, Either[(C, A), B]]] = F.map(x)(_.map(Right(_)))
    val q: F[A => Either[(C, A), B]] = F.map(y){ either =>
      (a: A) => either.bimap(c => (c, a), f => f(a))
    }
    val r: F[((C, A)) => B] = F.map(z)(f => Function.uncurried(f).tupled)

    val rhs: F[B] = p select q select r

    lhs <-> rhs
  }

  // TODO the law for when F is also a monad (must skip unnecessary effects)

}

object SelectiveLaws {
  def apply[F[_]](implicit ev: Selective[F]): SelectiveLaws[F] =
    new SelectiveLaws[F] { def F: Selective[F] = ev }
}
