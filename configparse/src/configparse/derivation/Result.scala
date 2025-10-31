package configparse.derivation

import configparse.model.Config
import configparse.model.Arr
import configparse.model.Str

sealed trait Result[+A]:
  def map[B](fn: A => B): Result[B]
  def flatMap[B](fn: A => Result[B]): Result[B]

object Result:

  case class Success[+A](value: A) extends Result[A]:
    def map[B](fn: A => B): Result[B] = Success(fn(value))
    def flatMap[B](fn: A => Result[B]): Result[B] = fn(value)

  case class Error(errors: FieldError*) extends Result[Nothing]:
    def map[B](fn: Nothing => B): Result[B] = this
    def flatMap[B](fn: Nothing => Result[B]): Result[B] = this

import configparse.model.Path
import configparse.model.Value
enum FieldError:

  // case Missing(path: Path)

  case TypeMismatch(path: Path, value: Value, expected: String)

  /** A requirement failed when instantiating a case class */
  case RequirementFailed(path: Path, value: Value, ex: IllegalArgumentException)

  def pretty = this match
    case TypeMismatch(path, value, expected) if value.origins.isEmpty =>
      s"missing required field '$path'"
    case TypeMismatch(path, value, expected) =>
      s"type mismatch in configuration '$path': found ${FieldError.shortTpe(value)}, expected $expected\n${FieldError.origin(value)}"
    case RequirementFailed(path, value, ex) =>
      s"validation error in configuration '$path': ${ex.getMessage()}\n${FieldError.origin(value)}"

object FieldError:
  private def shortTpe(value: Value) = value match
    case configparse.model.Null() => "<null>"
    case Str(s)                   =>
      val max = 10
      if s.length > max then s"'${s.take(max)}...'" else s"'$s'"
    case _: Arr    => "<config array>"
    case _: Config => "<config object>"

  private def origin(value: Value): String = value.origins match
    case Nil       => ""
    case head :: _ =>
      s"  at ${head.pretty}"
