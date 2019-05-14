package cats.selected.instances

import cats.instances.option.catsStdInstancesForOption
import cats.Selective

object option {
  implicit val catsStdSelectiveForOption = Selective.fromMonad[Option]
}
