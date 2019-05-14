package cats.free

import cats.arrow.FunctionK
import cats.{Applicative, Bifunctor, Functor, Selective, ~>}
import cats.selected.instances.option.catsStdSelectiveForOption
import cats.instances.either.catsStdBitraverseForEither
import cats.syntax.either._

/**
  * data Select f a where
  *   Pure :: a -> Select f a
  *   Select :: Select f (Either a b) -> f (a -> b) -> Select f b
  *
  * Free rigid selective functors. Rigidity is enforced
  * as the definition is based on the normal form that can
  * only be found for rigid selective functors.
  * This is to say that not all selective functors are free.
  * (A good thing, in most cases).
  */
sealed trait FreeSelective[F[_], A] {
  type Source
}
object FreeSelective {
  abstract class Pure[F[_], A] extends FreeSelective[F, A] {
    type Source = A
    val run: A
  }
  object Pure {
    def apply[F[_], A](x: A): Pure[F, A] = new Pure[F, A] {
      val run: A = x
    }
    def unapply[F[_], A](arg: Pure[F, A]): Option[A] = Option(arg.run)
    type Aux[F[_], A, B] = Pure[F, A] {
      type Source = B
    }
  }
  abstract class Select[F[_], B] extends FreeSelective[F, B] {
    type Source

    val feab: FreeSelective[F, Either[Source, B]]
    val fab: F[Source => B]
  }
  object Select {
    type Aux[F[_], A, B] = Select[F, B] {
      type Source = A
    }
  }

  implicit def selective[F[_]: Functor]: Selective[FreeSelective[F, ?]] = new Selective[FreeSelective[F, ?]] {
    override def applicative: Applicative[FreeSelective[F, ?]] = new Applicative[FreeSelective[F, ?]] {
      override def map[A, B](fa: FreeSelective[F, A])(f: A => B): FreeSelective[F, B] =
        fa match {
          case x: Pure[F, A] => Pure(f(x.run))
          case w: Select.Aux[F, A, B] =>
            new Select[F, B] {
              type Source = B
              /**
                *There is a bit of primitive recursion here that cannot be eliminated.
                * Since all free selective expressions are finite, this is guaranteed to terminate,
                * however it is not tail recursive, so your stack is your own.
                **/
              val feab: FreeSelective[F, Either[Source, B]] =
                map(w.feab)((e: Either[w.Source, B]) => e.leftMap(f))

              val fab: F[Source => B] = Functor[F].map(w.fab)(f andThen _)
            }
        }

      override def pure[A](x: A): FreeSelective[F, A] = Pure[F, A](x)

      /**
        * The canonical apS function agreement is enforced, since these are of
        * necessity rigid selective functors, since the normal form that
        * backs the free defintion only holds in the rigid case.
        */
      override def ap[A, B](ff: FreeSelective[F, A => B])(fa: FreeSelective[F, A]): FreeSelective[F, B] =
        select(map[A, Either[A, B]](fa)(Left.apply[A, B]))(ff)
    }

    /**
      * *
      * *  From the original paper:
      * *
      * * instance Functor f => Selective (Select f) where
      * *   select x (Pure y) = either y id <$> x -- Generalised identity
      * *   select x (Select y z) = Select (select (f <$> x) (g <$> y)) (h <$> z) -- Associativity
      * *     where
      * *       f x = Right <$> x
      * *       g y = \a -> bimap (,a) ($a) y
      * *       h z = uncurry z
      */
    override def select[A, B](fab: FreeSelective[F, Either[A, B]])(fn: FreeSelective[F, A => B]): FreeSelective[F, B] =
      fn match {
        case fn: Pure[F, A => B] =>
          map(fab)(_.fold(fn.run, identity))

        case fn: Select.Aux[F, A, A => B] =>
          val fx: FreeSelective[F, Either[A, Either[(A, A), B]]]  =
            map(fab)(e => Right(e.leftMap(a => (a, a))))

          val gy: FreeSelective[F, A => Either[(A, A), B]] =
            map(fn.feab)(
              (eit: Either[A, A => B]) =>
                (a: A) =>
                  Bifunctor[Either].bimap(eit)((_, a), f => f(a)))

          val hz: F[((A, A)) => B] =
            Functor[F].map(fn.fab)(
              Function.uncurried[A, A, B] _
                andThen Function.tupled[A, A, B])

          new Select[F, B] {
            type Source = (A, A)

            val fab: F[Source => B] = hz
            val feab: FreeSelective[F, Either[Source, B]] = select(fx)(gy)
          }
      }
  }

  /**
    * -- Extract all possible effects from a selective computation
    * getEffects :: Functor f => Select f a -> [f ()]
    * getEffects = getOver . runSelect (Over . pure . void)
    */
  def liftSelect[F[_]: Functor, A](fa: F[A]): FreeSelective[F, A] =
    new Select[F, A] {
      type Source = Unit

      val feab: FreeSelective[F, Either[Unit, A]] = Pure(Left())
      val fab: F[Unit => A] = Functor[F].map(fa)(Function.const)
    }

  def runSelect[F[_], G[_]: Selective, A, B](nat: F ~> G)(free: FreeSelective[F, A]): G[A] =
    free match {
      case x: Pure[F, A] => Selective[G].pure(x.run)
      case x: Select.Aux[F, A, B] =>
        Selective[G].select(runSelect(nat)(x.feab))(nat(x.fab))
    }

  private def toOptionK[F[_]] = new FunctionK[F, Option] {
    def apply[B](a: F[B]): Option[B] = None
  }
  def getPure[F[_], A](free: FreeSelective[F, A]): Option[A] =
    runSelect[F, Option, A, A](toOptionK[F])(free)
}
