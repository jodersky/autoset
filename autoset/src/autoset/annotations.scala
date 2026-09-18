package autoset

/** Marks a case class field as secret: its value is shown as `<secret>`
  * instead of its actual contents.
  */
class secret extends scala.annotation.StaticAnnotation

/** The key of a case class field in config objects, used as is instead of the
  * key given by `DerivedReaders.fieldName`.
  *
  * ```scala
  * case class Db(@name("db_host") host: String)
  * ```
  */
class name(val name: String) extends scala.annotation.StaticAnnotation

/** Former keys of a case class field in config objects, e.g. after renaming
  * it. They are still accepted, with a warning. If the field's key is also
  * set, it takes precedence and the deprecated keys are ignored.
  *
  * ```scala
  * case class Db(@deprecatedNames("hostname", "server") host: String)
  * ```
  */
class deprecatedNames(val names: String*) extends scala.annotation.StaticAnnotation

/** Read a case class field with `reader`, instead of the given reader for its
  * type. The reader must be a stable reference, such as a member of an
  * object, and read the field's type.
  *
  * ```scala
  * object Hex:
  *   val int: autoset.Reader[Int] = ...
  *
  * case class Color(@readWith(Hex.int) rgb: Int)
  * ```
  */
class readWith(val reader: autoset.derivation.ReadersApi#Reader[?])
    extends scala.annotation.StaticAnnotation
