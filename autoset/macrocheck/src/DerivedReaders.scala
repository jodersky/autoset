package macrocheck

import autoset.derivation.DefaultReaders
import autoset.secret

// Uses of `readerFor` covering every kind of field it generates code for.
// Compiling this is the check; nothing is run.

case class Plain(s: String, i: Int, d: Double, b: Boolean, o: Option[Int], p: os.Path)
case class Defaults(host: String, port: Int = 5432, tags: List[String] = Nil)
case class Secrets(@secret password: String, @secret token: String = "")
case class Nested(name: String, plain: Plain, defaults: Defaults)
case class Collections(list: List[Int], set: Set[String], map: Map[String, Vector[Double]])
case class Generic[T](value: T, count: Int = 1)
case class Recursive(name: String, children: List[Recursive] = Nil)
case class Empty()

object readers extends DefaultReaders:
  given Reader[Plain] = readerFor[Plain]
  given Reader[Defaults] = readerFor[Defaults]
  given Reader[Secrets] = readerFor[Secrets]
  given Reader[Nested] = readerFor[Nested]
  given Reader[Collections] = readerFor[Collections]
  given Reader[Recursive] = readerFor[Recursive]
  given Reader[Empty] = readerFor[Empty]
  given intGeneric: Reader[Generic[Int]] = readerFor[Generic[Int]]
  given nestedGeneric: Reader[Generic[Generic[Int]]] = readerFor[Generic[Generic[Int]]]

// called from outside the instance, with its readers found through its path
val outside = readers.readerFor[Defaults]

// called on an instance of a class rather than an object
class Readers extends DefaultReaders
val instance = Readers()
val onInstance = instance.readerFor[Plain]

// derived through the `derived` extension on `Reader`
case class DerivedDefault(host: String, port: Int = 1, @secret key: String = "") derives autoset.Reader
case class DerivedNested(inner: DerivedDefault, list: List[DerivedDefault]) derives autoset.Reader
case class DerivedInstance(plain: Plain) derives readers.Reader

// sealed types, enums and string literal unions
enum CheckLevel derives autoset.Reader:
  case Debug, Info
sealed trait CheckStorage derives autoset.Reader
case class CheckDisk(path: os.Path, sync: Boolean = false) extends CheckStorage
case object CheckMemory extends CheckStorage
sealed trait CheckNested extends CheckStorage
case class CheckS3(@secret key: String) extends CheckNested
enum CheckShape derives autoset.Reader:
  case Circle(radius: Double)
  case Point
type CheckMode = "a" | "b"
val modeReader = readers.readerFor[CheckMode]
case class CheckSettings(storage: CheckStorage, level: CheckLevel, shape: CheckShape) derives autoset.Reader
