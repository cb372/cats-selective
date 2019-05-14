package cats.syntax

import cats.Selective

class catsSyntaxForSelectiveEither[F[_]: Selective, A, B](fa: F[Either[A, B]]) {
  def select(ff: F[A => B]): F[B] =
    Selective[F].select(fa)(ff)

  def branch[C](l: F[A => C])(r: F[B => C]): F[C] =
    Selective[F].branch(fa)(l)(r)

  def <*?(ff: F[A => B]): F[B] =
    Selective[F].select(fa)(ff)
}

class catsStdSyntaxForSelectiveEither[F[_]: Selective, A, B](ff: F[A => B]) {
  def ?*>(fab: F[Either[A, B]]): F[B] =
    Selective[F].select(fab)(ff)
}

class catsSyntaxForSelectiveBoolean[F[_]: Selective](fbool: F[Boolean]) {
  def ifS[A](t: F[A])(e: F[A]): F[A] =
    Selective[F].ifS(fbool)(t)(e)

  def <||>(fother: F[Boolean]): F[Boolean] =
    Selective[F].ifS(fbool)(Selective[F].pure(true))(fother)

  def <&&>(fother: F[Boolean]): F[Boolean] =
    Selective[F].ifS(fbool)(fother)(Selective[F].pure(false))
}
