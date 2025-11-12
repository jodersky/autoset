package configparse.derivation

import configparse.model.Config
import configparse.model.Arr
import configparse.model.Str
import configparse.model.Path
import configparse.model.Value

case class Invalid(
    path: Path,
    value: Value,
    message: String
)

enum Result[+A]:
  case Success(value: A)
  case Error(
      missing: Seq[Path] = Seq(),
      invalid: Seq[Invalid] = Seq()
  ) extends Result[Nothing]

  def map[B](fn: A => B): Result[B] = this match
    case Success(value) => Success(fn(value))
    case e: Error       => e

  def flatMap[B](fn: A => Result[B]): Result[B] = this match
    case Success(value) => fn(value)
    case e: Error       => e
