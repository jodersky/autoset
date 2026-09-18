package autoset.derivation

import autoset.model.{Arr, Null, Obj, Reporter, Str, Value}

trait ReadersApi:

  trait Reader[A]:

    /** Read a value and parse it as an `A`.
      *
      * On failure, report at least one error and return `None`. Readers may
      * mark parts of `value`, e.g. as secret.
      */
    def read(
      value: Value,
      path: Vector[String],
      reporter: Reporter
    ): Option[A]

  /** Companion of `Reader`, so that it can be used in `derives` clauses.
    * The `derived` method itself is added as an extension by
    * `DerivedReaders`.
    */
  object Reader

  /** Convert a config object into a scala type, emitting any error and/or
    * warnings.
    */
  def project[A](obj: Value, reporter: Reporter)(using reader: Reader[A]): Option[A] =
    reader.read(obj, Vector.empty, reporter)

/** Helpers for writing readers. */
object ReaderUtils:

  /** A short description of a value's type, for error messages. Secrets are
    * not described.
    */
  def describe(value: Value): String = value match
    case _ if value.secret => "<secret>"
    case _: Obj => "an object"
    case _: Arr => "an array"
    case _: Null => "null"
    case Str(raw, _, _) => s"'$raw'"

  /** Report that the value at `path` is not what was expected. */
  def mismatch(
      expected: String,
      value: Value,
      path: Vector[String],
      reporter: Reporter
  ): None.type =
    val at = if path.isEmpty then "" else s" for '${path.mkString(".")}'"
    reporter.error(s"expected $expected$at, found ${describe(value)}", value.effectiveOrigin)
    None

  /** `thisIsKebabCase => this-is-kebab-case` */
  def kebabify(camelCase: String): String = separate(camelCase, '-')

  /** `thisIsSnakeCase => this_is_snake_case` */
  def snakify(camelCase: String): String = separate(camelCase, '_')

  /** Lower-case `camelCase`, with `sep` between words. */
  private def separate(camelCase: String, sep: Char): String =
    val sb = StringBuilder()
    var prevIsLower = false
    for c <- camelCase do
      if prevIsLower && c.isUpper then sb += sep
      sb += c.toLower
      prevIsLower = c.isLower
    sb.result()
