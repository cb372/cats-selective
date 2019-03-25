package cats.selected.instances

import cats.{Applicative, Selective, Semigroup}
import cats.data.Validated
import cats.data.Validated._

object validated {

  implicit def catsDataSelectiveForValidated[E](implicit E: Semigroup[E]): Selective[Validated[E, ?]] =
    new Selective[Validated[E, ?]] {

      def applicative: Applicative[Validated[E, ?]] = catsDataApplicativeErrorForValidated[E]

      def select[A, B](fab: Validated[E, scala.Either[A, B]])(f: Validated[E, A => B]): Validated[E, B] = fab.andThen {
        case Right(b) => Valid(b)
        case Left(a)  => f.map(fn => fn(a))
      }

    }

}
