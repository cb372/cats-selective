package cats

// TODO simulacrum bug? @typeclass doesn't work
trait Selective[F[_]] {
  def applicative: Applicative[F]

  def select[A, B](fab: F[Either[A, B]])(fn: F[A => B]): F[B]

  def pure[A](a: A): F[A] = applicative.pure(a)

  def map[A, B](fa: F[A])(f: A => B): F[B] = applicative.map(fa)(f)

  def branch[A, B, C](x: F[Either[A, B]])(l: F[A => C])(r: F[B => C]): F[C] = {
    val lhs = {
      val innerLhs: F[Either[A, Either[B, C]]] = map(x)(_.right.map(Left(_)))
      val innerRhs: F[A => Either[B, C]] = map(l)(_.andThen(Right(_)))
      select(innerLhs)(innerRhs)
    }
    select(lhs)(r)
  }

  /**
    * apS :: Selective f => f (a -> b) -> f a -> f b
    * apS f x = select (Left <$> f) ((&) <$> x)
    *
    *  (&) :: c -> (c -> d) -> d     -- reverse function application
    *
    * Although the type signature of `apS` matches `applicative.ap`, in general,
    * they are *not* equivalent.  Selective functors that *do* satisfy the
    * property `applicative.ap === apS` are called *rigid* selectives.
    */
  def apS[A, B](fn: F[A => B])(fa: F[A]): F[B] = {
    val leftFn: F[Either[A => B, B]] = map(fn)(Left(_))
    val application = map(fa)(a => (g: A => B) => g(a))
    select(leftFn)(application)
  }

  def ifS[A](x: F[Boolean])(t: F[A])(e: F[A]): F[A] = {
    val condition: F[Either[Unit, Unit]] = map(x)(p => if (p) Left(()) else Right(()))
    val left: F[Unit => A] = map(t)(Function.const)
    val right: F[Unit => A] = map(e)(Function.const)
    branch(condition)(left)(right)
  }

  def whenS[A](fbool: F[Boolean])(fa: F[Unit]): F[Unit] =
    ifS(fbool)(fa)(pure(()))

  def bindBool[A](fbool: F[Boolean])(f: Boolean => F[A]): F[A] =
    ifS(fbool)(f(false))(f(true))

  def fromMaybeS[A](fa: F[A])(fm: F[Option[A]]): F[A] =
    select(
      applicative.map[Option[A], Either[Unit, A]](fm)(_.toRight(left = ()))
    )(applicative.map(fa)(a => Function.const(a)))

  def orS(fbool: F[Boolean])(fa: F[Boolean]): F[Boolean] =
    ifS(fbool)(pure(true))(fa)

  def andS(fbool: F[Boolean])(fa: F[Boolean]): F[Boolean] =
    ifS(fbool)(fa)(pure(false))

  def anyS[G[_]: Foldable, A](test: A => F[Boolean])(ga: G[A]): Eval[F[Boolean]] =
    Foldable[G].foldRight(ga, Eval.now(pure(false)))({
      (a: A, lb: Eval[F[Boolean]]) =>
        lb.map(orS(_)(test(a)))
    })

  def allS[G[_]: Foldable, A](test: A => F[Boolean])(ga: G[A]): Eval[F[Boolean]] =
    Foldable[G].foldRight(ga, Eval.now(pure(true)))({
      (a: A, lb: Eval[F[Boolean]]) =>
        lb.map(andS(_)(test(a)))
    })

  // TODO more combinators here
}

object Selective {
  def apply[F[_]](implicit ev: Selective[F]): Selective[F] = ev

  def fromMonad[F[_]](implicit M: Monad[F]): Selective[F] =
    new Selective[F] {
      val applicative: Applicative[F] = M
      def select[A, B](fa: F[Either[A, B]])(fn: F[A => B]): F[B] =
        M.flatMap(fa) {
          case Right(b) => M.pure(b)
          case Left(a)  => M.map(fn)(_(a))
        }
    }

  object ops {

    implicit class SelectiveOps[F[_], A, B](target: F[Either[A, B]])(implicit F: Selective[F]) {

      def select(fn: F[A => B]): F[B] = F.select(target)(fn)

      def <*?(fn: F[A => B]): F[B] = F.select(target)(fn)

    }

  }

}
