package test

import autoset.derivation.DefaultReaders
import autoset.model.*
import autoset.secret
import utest.*

import Helpers.*

case class Db(host: String, port: Int = 5432, @secret password: String = "")
case class App(name: String, db: Db, tags: List[String] = Nil)
case class Box[T](value: T, count: Int = 1)
case class Tree(name: String, children: List[Tree] = Nil)
case class Empty()
case class NoReader(x: Thread)

// derived with the default readers of the `autoset` package
case class Server(host: String, port: Int = 8080, @secret key: String = "") derives autoset.Reader
case class Service(name: String, server: Server) derives autoset.Reader

// derived with a specific instance of readers
case class Client(url: String, retries: Int = 3) derives DerivedReadersTest.readers.Reader
case class UsesClient(client: Client)

case class Optional(name: String, nick: Option[String], age: Option[Int] = Some(18))
case class Pool(maxSize: Int, minIdle: Int = 0)
case class Database(jdbcUrl: String, connectionPool: Pool)

enum Level:
  case Debug, Info, Warn

sealed trait Storage
case class Disk(path: String, sync: Boolean = false) extends Storage
case class S3(bucket: String, @secret key: String = "") extends Storage
case object Memory extends Storage

enum Shape:
  case Circle(radius: Double)
  case Rect(width: Double, height: Double = 1)
  case Point

sealed trait Animal
sealed trait Pet extends Animal
case class Dog(name: String) extends Pet
case object Cat extends Pet
case class Wolf(pack: Int) extends Animal

type Mode = "fast" | "safe"

case class Settings(storage: Storage, level: Level = Level.Info, mode: Mode = "safe")

enum Color derives autoset.Reader:
  case Red, Green, Blue

sealed trait NotAllCases
class Plain extends NotAllCases

sealed trait Duplicated
object First { case class Same() extends Duplicated }
object Second { case class Same() extends Duplicated }

sealed trait Generic[T]
case class GenericCase[T](value: T) extends Generic[T]

object DerivedReadersTest extends TestSuite:

  object readers extends DefaultReaders:
    given Reader[Db] = readerFor[Db]
    given Reader[App] = readerFor[App]
    given Reader[Tree] = readerFor[Tree]
    given Reader[UsesClient] = readerFor[UsesClient]
    given Reader[Optional] = readerFor[Optional]
    given Reader[Level] = readerFor[Level]
    given Reader[Storage] = readerFor[Storage]
    given Reader[Shape] = readerFor[Shape]
    given Reader[Animal] = readerFor[Animal]
    given Reader[Mode] = readerFor[Mode]
    given Reader[Settings] = readerFor[Settings]

  import readers.{Reader, given}

  /** Readers using snake_case keys in config files. */
  object snakeReaders extends DefaultReaders:
    override def fieldName(name: String) = autoset.derivation.ReaderUtils.snakify(name)
    given Reader[Pool] = readerFor[Pool]
    given Reader[Database] = readerFor[Database]

  /** Read `value` at the root, returning the result, the value to show and any output. */
  def read[A](value: Value)(using reader: Reader[A]): (Option[(A, Value)], String) =
    val out = java.io.ByteArrayOutputStream()
    val reporter = Reporter.printing(java.io.PrintStream(out))
    val result = reader.read(value, Vector.empty, reporter)
    assert(result.isEmpty == reporter.hasErrors)
    (result, out.toString)

  def s(raw: String, line: Int = 1) = str(raw, file(line))

  val tests = Tests {
    test("fields") {
      val v = obj(file(1))("host" -> s("localhost"), "port" -> s("1234"), "password" -> s("pw"))
      val (result, out) = read[Db](v)
      assert(result.get._1 == Db("localhost", 1234, "pw"))
      assert(out == "")
    }
    test("defaults") {
      val (result, out) = read[Db](obj(file(1))("host" -> s("localhost")))
      assert(result.get._1 == Db("localhost", 5432, ""))
      assert(out == "")
    }
    test("missing field") {
      val (result, out) = read[Db](obj(file(1))("port" -> s("1")))
      assert(result.isEmpty)
      assert(out == "error: app.conf:1:1: missing required field 'host'\n")
    }
    test("all errors are reported") {
      val (result, out) = read[Db](obj(file(1))("port" -> s("x", 2)))
      assert(result.isEmpty)
      assert(
        out ==
          """error: app.conf:1:1: missing required field 'host'
            |error: app.conf:2:1: expected an integer for 'port', found 'x'
            |""".stripMargin
      )
    }
    test("unknown keys") {
      val v = obj(file(1))("host" -> s("h"), "hots" -> s("x", 2), "extra" -> obj(file(3))())
      val (result, out) = read[Db](v)
      assert(result.get._1 == Db("h"))
      assert(
        out ==
          """warning: app.conf:2:1: unknown key 'hots'
            |warning: app.conf:3:1: unknown key 'extra'
            |""".stripMargin
      )
    }
    test("not an object") {
      val (result, out) = read[Db](s("x"))
      assert(result.isEmpty)
      assert(out == "error: app.conf:1:1: expected an object, found 'x'\n")
    }
    test("nested") {
      val v = obj(file(1))(
        "name" -> s("app"),
        "db" -> obj(file(2))("host" -> s("h", 2), "port" -> s("x", 3), "typo" -> s("", 4)),
        "tags" -> arr(file(5))(s("a", 5), s("b", 5))
      )
      val (result, out) = read[App](v)
      assert(result.isEmpty)
      assert(
        out ==
          """error: app.conf:3:1: expected an integer for 'db.port', found 'x'
            |warning: app.conf:4:1: unknown key 'db.typo'
            |""".stripMargin
      )

      val good = obj(file(1))("name" -> s("app"), "db" -> obj(file(2))("host" -> s("h", 2)))
      assert(read[App](good)._1.get._1 == App("app", Db("h"), Nil))
    }
    test("missing nested field") {
      val (result, out) = read[App](obj(file(1))("name" -> s("app"), "db" -> obj(file(2))()))
      assert(result.isEmpty)
      assert(out == "error: app.conf:2:1: missing required field 'db.host'\n")
    }
    test("shown value") {
      val v = obj(file(1))("host" -> s("h"), "password" -> s("hunter2", 2), "extra" -> s("x"))
      val (result, _) = read[Db](v)
      // secrets are hidden, unknown keys are dropped
      assert(
        result.get._2 == obj(file(1))(
          "host" -> s("h"),
          "password" -> Str("<secret>", LitKind.String, List(file(2)))
        )
      )
    }
    test("generic") {
      given intBox: Reader[Box[Int]] = readers.readerFor[Box[Int]]
      assert(read[Box[Int]](obj(file(1))("value" -> s("3")))._1.get._1 == Box(3, 1))
      given listBox: Reader[Box[List[String]]] = readers.readerFor[Box[List[String]]]
      val v = obj(file(1))("value" -> arr(file(1))(s("a")), "count" -> s("2"))
      assert(read[Box[List[String]]](v)._1.get._1 == Box(List("a"), 2))
    }
    test("recursive") {
      val v = obj(file(1))(
        "name" -> s("root"),
        "children" -> arr(file(1))(obj(file(1))("name" -> s("leaf")))
      )
      assert(read[Tree](v)._1.get._1 == Tree("root", List(Tree("leaf"))))
    }
    test("empty") {
      given Reader[Empty] = readers.readerFor[Empty]
      assert(read[Empty](obj(file(1))())._1.get._1 == Empty())
    }
    test("readers are looked up on the instance") {
      // the imported givens are of type `readers.Reader`, so the field readers
      // can only come from `other`, through the path-dependent type `other.Reader`
      object other extends DefaultReaders
      val reader = other.readerFor[Db]
      val out = java.io.ByteArrayOutputStream()
      val reporter = Reporter.printing(java.io.PrintStream(out))
      val result = reader.read(obj(file(1))("host" -> s("h")), Vector.empty, reporter)
      assert(result.get._1 == Db("h"))
    }
    test("derives") {
      def readDefault[A](value: Value)(using reader: autoset.Reader[A]) =
        val out = java.io.ByteArrayOutputStream()
        (reader.read(value, Vector.empty, Reporter.printing(java.io.PrintStream(out))), out.toString)

      val (result, out) = readDefault[Server](obj(file(1))("host" -> s("h"), "key" -> s("k", 2)))
      assert(result.get._1 == Server("h", 8080, "k"))
      assert(result.get._2 == obj(file(1))(
        "host" -> s("h"),
        "key" -> Str("<secret>", LitKind.String, List(file(2)))
      ))
      assert(out == "")
    }
    test("derives nested") {
      // the reader for `server` is the one derived for `Server`
      val reader = summon[autoset.Reader[Service]]
      val out = java.io.ByteArrayOutputStream()
      val v = obj(file(1))("name" -> s("svc"), "server" -> obj(file(2))("port" -> s("x", 3)))
      val result = reader.read(v, Vector.empty, Reporter.printing(java.io.PrintStream(out)))
      assert(result.isEmpty)
      assert(out.toString.linesIterator.toList == List(
        "error: app.conf:2:1: missing required field 'server.host'",
        "error: app.conf:3:1: expected an integer for 'server.port', found 'x'"
      ))
    }
    test("derives with an instance") {
      assert(read[Client](obj(file(1))("url" -> s("u")))._1.get._1 == Client("u", 3))
      // a derived reader is found for fields of other derived readers
      val v = obj(file(1))("client" -> obj(file(1))("url" -> s("u"), "retries" -> s("5")))
      assert(read[UsesClient](v)._1.get._1 == UsesClient(Client("u", 5)))
    }
    test("optional fields") {
      // missing options are `None`, unless they have a default
      assert(read[Optional](obj(file(1))("name" -> s("n")))._1.get._1 == Optional("n", None, Some(18)))
      val v = obj(file(1))("name" -> s("n"), "nick" -> s("x"), "age" -> nul(file(1)))
      assert(read[Optional](v)._1.get._1 == Optional("n", Some("x"), None))
      val (result, out) = read[Optional](obj(file(1))("nick" -> s("x"), "age" -> s("old", 2)))
      assert(result.isEmpty)
      assert(
        out ==
          """error: app.conf:1:1: missing required field 'name'
            |error: app.conf:2:1: expected an integer for 'age', found 'old'
            |""".stripMargin
      )
    }
    test("field names") {
      import autoset.derivation.ReaderUtils.{kebabify, snakify}
      assert(snakify("maxSize") == "max_size")
      assert(snakify("jdbcURL") == "jdbc_url")
      assert(snakify("already_snake") == "already_snake")
      assert(kebabify("connectionPool") == "connection-pool")
      assert(kebabify("x") == "x")
    }
    test("overridden field names") {
      def readSnake[A](value: Value)(using reader: snakeReaders.Reader[A]) =
        val out = java.io.ByteArrayOutputStream()
        (reader.read(value, Vector.empty, Reporter.printing(java.io.PrintStream(out))), out.toString)

      val v = obj(file(1))(
        "jdbc_url" -> s("jdbc:h2:mem"),
        "connection_pool" -> obj(file(2))("max_size" -> s("10", 2))
      )
      val (result, out) = readSnake[Database](v)
      assert(result.get._1 == Database("jdbc:h2:mem", Pool(10, 0)))
      assert(out == "")
      // the shown value uses the keys of the config
      assert(result.get._2 == v)

      // the Scala names are not keys anymore, and errors use config keys
      val camel = obj(file(1))(
        "jdbcUrl" -> s("x", 2),
        "connection_pool" -> obj(file(3))("maxSize" -> s("10", 4))
      )
      val (failed, errors) = readSnake[Database](camel)
      assert(failed.isEmpty)
      assert(
        errors ==
          """error: app.conf:1:1: missing required field 'jdbc_url'
            |error: app.conf:3:1: missing required field 'connection_pool.max_size'
            |warning: app.conf:4:1: unknown key 'connection_pool.maxSize'
            |warning: app.conf:2:1: unknown key 'jdbcUrl'
            |""".stripMargin
      )
    }
    test("enum") {
      assert(read[Level](s("Info"))._1.get._1 == Level.Info)
      assert(read[Level](s(" Warn "))._1.get._1 == Level.Warn)
      // the long form works too
      assert(read[Level](obj(file(1))("type" -> s("Debug")))._1.get._1 == Level.Debug)
      assert(
        read[Level](s("info"))._2 ==
          "error: app.conf:1:1: expected one of 'Debug', 'Info', 'Warn', found 'info'\n"
      )
      assert(
        read[Level](arr(file(1))())._2 ==
          "error: app.conf:1:1: expected one of 'Debug', 'Info', 'Warn', found an array\n"
      )
    }
    test("sealed trait") {
      val disk = obj(file(1))("type" -> s("Disk"), "path" -> s("/d", 2))
      val (result, out) = read[Storage](disk)
      assert(result.get._1 == Disk("/d"))
      assert(out == "")
      // the discriminator is part of the shown value
      assert(result.get._2 == disk)

      assert(read[Storage](obj(file(1))("type" -> s("S3"), "bucket" -> s("b")))._1.get._1 == S3("b"))
      // case objects are written as their name, or as an object
      assert(read[Storage](s("Memory"))._1.get._1 == Memory)
      assert(read[Storage](obj(file(1))("type" -> s("Memory")))._1.get._1 == Memory)
    }
    test("sealed trait errors") {
      def errors(v: Value) = read[Storage](v)._2

      assert(errors(obj(file(1))("path" -> s("/d"))) == "error: app.conf:1:1: missing required field 'type'\n")
      assert(
        errors(obj(file(1))("type" -> s("Tape", 2))) ==
          "error: app.conf:2:1: expected one of 'Disk', 'S3', 'Memory' for 'type', found 'Tape'\n"
      )
      assert(
        errors(obj(file(1))("type" -> obj(file(2))())) ==
          "error: app.conf:2:1: expected one of 'Disk', 'S3', 'Memory' for 'type', found an object\n"
      )
      // the string form can't give required fields
      assert(errors(s("Disk")) == "error: app.conf:1:1: missing required field 'path'\n")
      assert(errors(s("Tape")) == "error: app.conf:1:1: expected one of 'Disk', 'S3', 'Memory', found 'Tape'\n")
      assert(errors(arr(file(1))()) == "error: app.conf:1:1: expected an object, found an array\n")
      // errors and warnings of the case itself
      assert(
        errors(obj(file(1))("type" -> s("Disk"), "path" -> s("/d"), "sync" -> s("maybe", 2), "x" -> s("", 3))) ==
          """error: app.conf:2:1: expected a boolean ('true' or 'false') for 'sync', found 'maybe'
            |warning: app.conf:3:1: unknown key 'x'
            |""".stripMargin
      )
      assert(
        errors(obj(file(1))("type" -> s("Memory"), "size" -> s("1", 2))) ==
          "warning: app.conf:2:1: unknown key 'size'\n"
      )
    }
    test("secret in a case") {
      val v = obj(file(1))("type" -> s("S3"), "bucket" -> s("b"), "key" -> s("k", 2))
      assert(read[Storage](v)._1.get._2 == obj(file(1))(
        "type" -> s("S3"),
        "bucket" -> s("b"),
        "key" -> Str("<secret>", LitKind.String, List(file(2)))
      ))
    }
    test("enum with parameters") {
      assert(read[Shape](obj(file(1))("type" -> s("Circle"), "radius" -> s("2")))._1.get._1 == Shape.Circle(2))
      assert(read[Shape](obj(file(1))("type" -> s("Rect"), "width" -> s("3")))._1.get._1 == Shape.Rect(3, 1))
      assert(read[Shape](s("Point"))._1.get._1 == Shape.Point)
    }
    test("nested sealed traits") {
      // cases are flattened
      assert(read[Animal](obj(file(1))("type" -> s("Dog"), "name" -> s("rex")))._1.get._1 == Dog("rex"))
      assert(read[Animal](s("Cat"))._1.get._1 == Cat)
      assert(read[Animal](obj(file(1))("type" -> s("Wolf"), "pack" -> s("3")))._1.get._1 == Wolf(3))
    }
    test("string literals") {
      assert(read[Mode](s("fast"))._1.get._1 == "fast")
      assert(read[Mode](s(" safe "))._1.get._1 == "safe")
      assert(read[Mode](s("slow"))._2 == "error: app.conf:1:1: expected one of 'fast', 'safe', found 'slow'\n")
    }
    test("sums as fields") {
      val v = obj(file(1))(
        "storage" -> obj(file(2))("type" -> s("Disk", 2), "path" -> s("/d", 2)),
        "level" -> s("Debug", 3),
        "mode" -> s("fast", 4)
      )
      assert(read[Settings](v)._1.get._1 == Settings(Disk("/d"), Level.Debug, "fast"))
      val bad = obj(file(1))("storage" -> obj(file(2))("type" -> s("Tape", 3)), "level" -> s("Trace", 4))
      assert(
        read[Settings](bad)._2 ==
          """error: app.conf:3:1: expected one of 'Disk', 'S3', 'Memory' for 'storage.type', found 'Tape'
            |error: app.conf:4:1: expected one of 'Debug', 'Info', 'Warn' for 'level', found 'Trace'
            |""".stripMargin
      )
    }
    test("overridden case names and discriminator") {
      object kebabReaders extends DefaultReaders:
        override def caseName(name: String) = autoset.derivation.ReaderUtils.kebabify(name)
        override def discriminator = "kind"
        given Reader[Storage] = readerFor[Storage]
        given Reader[Level] = readerFor[Level]
      def readKebab[A](v: Value)(using r: kebabReaders.Reader[A]) =
        val out = java.io.ByteArrayOutputStream()
        (r.read(v, Vector.empty, Reporter.printing(java.io.PrintStream(out))).map(_._1), out.toString)

      assert(readKebab[Level](s("debug")) == (Some(Level.Debug), ""))
      assert(readKebab[Storage](obj(file(1))("kind" -> s("s3"), "bucket" -> s("b"))) == (Some(S3("b")), ""))
      assert(
        readKebab[Storage](obj(file(1))("type" -> s("s3"))) ==
          (None, "error: app.conf:1:1: missing required field 'kind'\n")
      )
    }
    test("derives on an enum") {
      val out = java.io.ByteArrayOutputStream()
      val result = summon[autoset.Reader[Color]].read(s("Green"), Vector.empty, Reporter.printing(java.io.PrintStream(out)))
      assert(result.get._1 == Color.Green)
    }
    test("compile errors") {
      import scala.compiletime.testing.typeCheckErrors
      val notCaseClass = typeCheckErrors("readers.readerFor[String]").map(_.message)
      assert(
        notCaseClass == List(
          "cannot derive a reader for String, which is not a case class, sealed type, enum or union of string literals"
        )
      )
      assert(
        typeCheckErrors("readers.readerFor[Duplicated]").map(_.message) ==
          List("cannot derive a reader for Duplicated, since more than one of its cases is called Same")
      )
      assert(
        typeCheckErrors("readers.readerFor[Generic[Int]]").map(_.message) ==
          List("cannot derive a reader for Generic[Int], since generic sealed types are not supported")
      )
      val notAllCases = typeCheckErrors("readers.readerFor[NotAllCases]").map(_.message)
      assert(
        notAllCases == List(
          "cannot derive a reader for NotAllCases, since its subtype Plain is not a case class, case object or sealed"
        )
      )
      val noReader = typeCheckErrors("readers.readerFor[NoReader]").map(_.message)
      assert(noReader.size == 1)
      assert(noReader.head.startsWith("no given instance of Reader[Thread] found for field 'x' of NoReader"))
    }
  }
