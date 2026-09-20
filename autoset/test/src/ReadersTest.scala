package test

import autoset.derivation.DefaultReaders
import autoset.model.*
import utest.*

import Helpers.*

object ReadersTest extends TestSuite:

  object readers extends DefaultReaders
  import readers.{Reader, given}

  /** Read `value` at path `a.b`, returning the result and any output. */
  def read[A](value: Value)(using reader: Reader[A]): (Option[A], String) =
    val out = java.io.ByteArrayOutputStream()
    val reporter = Reporter.printing(java.io.PrintStream(out))
    val result = reader.read(value, None, Context(Vector("a", "b"), reporter))
    assert(result.isEmpty == reporter.hasErrors)
    (result, out.toString)

  def ok[A: Reader](raw: String): A =
    val (result, out) = read[A](str(raw, file(1)))
    assert(out == "")
    result.get

  /** The error message for reading `raw`, without location. */
  def err[A: Reader](raw: String): String = err[A](str(raw, file(1)))

  def err[A: Reader](value: Value): String =
    val (result, out) = read[A](value)
    assert(result.isEmpty)
    out.stripPrefix("error: app.conf:1:1: ").stripSuffix("\n")

  val tests = Tests {
    test("value") {
      val v = obj(file(1))("x" -> str("1", file(1)))
      assert(read[Value](v)._1.get eq v)
    }
    test("obj") {
      val v = obj(file(1))()
      assert(read[Obj](v)._1.get eq v)
      assert(err[Obj]("x") == "expected an object for 'a.b', found 'x'")
    }
    test("arr") {
      val v = arr(file(1))(str("x", file(1)))
      assert(read[Arr](v)._1.get eq v)
      assert(err[Arr](obj(file(1))()) == "expected an array for 'a.b', found an object")
    }
    test("str") {
      val v = str("x", file(1))
      assert(read[Str](v)._1.get eq v)
      assert(err[Str](nul(file(1))) == "expected a string for 'a.b', found null")
    }
    test("null") {
      val v = nul(file(1))
      assert(read[Null](v)._1.get eq v)
      assert(err[Null]("x") == "expected null for 'a.b', found 'x'")
    }
    test("wrong type") {
      assert(err[String](obj(file(1))()) == "expected a string for 'a.b', found an object")
      assert(err[String](arr(file(1))()) == "expected a string for 'a.b', found an array")
      assert(err[Int](nul(file(1))) == "expected an integer for 'a.b', found null")
      assert(err[Boolean](nul(file(1))) == "expected a boolean for 'a.b', found null")
      assert(err[Double](nul(file(1))) == "expected a number for 'a.b', found null")
    }
    test("root path") {
      val out = java.io.ByteArrayOutputStream()
      readers.IntReader.read(str("x", env("X")), None, Context(Reporter.printing(java.io.PrintStream(out))))
      assert(out.toString == "error: env X: expected an integer, found 'x'\n")
    }
    test("string") {
      assert(ok[String]("hello") == "hello")
      assert(ok[String](" padded ") == " padded ")
      assert(ok[String]("") == "")
    }
    test("int") {
      assert(ok[Int]("42") == 42)
      assert(ok[Int](" -7 ") == -7)
      assert(ok[Int]("+3") == 3)
      assert(ok[Int]("2147483647") == Int.MaxValue)
      assert(ok[Int]("-2147483648") == Int.MinValue)
      assert(
        err[Int]("2147483648") ==
          "expected an integer between -2147483648 and 2147483647 for 'a.b', found '2147483648'"
      )
      assert(err[Int]("1.5") == "expected an integer for 'a.b', found '1.5'")
      assert(err[Int]("abc") == "expected an integer for 'a.b', found 'abc'")
      assert(err[Int]("") == "expected an integer for 'a.b', found ''")
    }
    test("byte") {
      assert(ok[Byte]("127") == Byte.MaxValue)
      assert(ok[Byte]("-128") == Byte.MinValue)
      assert(err[Byte]("128") == "expected an integer between -128 and 127 for 'a.b', found '128'")
    }
    test("short") {
      assert(ok[Short]("300") == 300.toShort)
      assert(
        err[Short]("40000") ==
          "expected an integer between -32768 and 32767 for 'a.b', found '40000'"
      )
    }
    test("long") {
      assert(ok[Long]("99999999999") == 99999999999L)
      assert(ok[Long]("9223372036854775807") == Long.MaxValue)
      assert(err[Long]("9223372036854775808").startsWith("expected an integer between"))
    }
    test("double") {
      assert(ok[Double]("1.5") == 1.5)
      assert(ok[Double](" 42 ") == 42.0)
      assert(ok[Double]("-1e3") == -1000.0)
      assert(ok[Double]("NaN").isNaN)
      assert(ok[Double]("Infinity") == Double.PositiveInfinity)
      assert(ok[Double]("-Infinity") == Double.NegativeInfinity)
      assert(err[Double]("1e400") == "expected a number within range for 'a.b', found '1e400'")
      assert(err[Double]("abc") == "expected a number for 'a.b', found 'abc'")
      assert(err[Double]("1d") == "expected a number for 'a.b', found '1d'")
      assert(err[Double]("1f") == "expected a number for 'a.b', found '1f'")
    }
    test("float") {
      assert(ok[Float]("1.5") == 1.5f)
      assert(err[Float]("1e50") == "expected a number within range for 'a.b', found '1e50'")
      assert(err[Float]("2F") == "expected a number for 'a.b', found '2F'")
    }
    test("boolean") {
      assert(ok[Boolean]("true"))
      assert(!ok[Boolean]("false"))
      assert(ok[Boolean]("TRUE"))
      assert(ok[Boolean](" True "))
      assert(
        err[Boolean]("yes") == "expected a boolean ('true' or 'false') for 'a.b', found 'yes'"
      )
    }
    test("char") {
      assert(ok[Char]("x") == 'x')
      assert(ok[Char](" ") == ' ')
      assert(err[Char]("xy") == "expected a single character for 'a.b', found 'xy'")
      assert(err[Char]("") == "expected a single character for 'a.b', found ''")
    }
    test("duration") {
      import scala.concurrent.duration.*
      assert(ok[Duration]("10 seconds") == 10.seconds)
      assert(ok[Duration]("5m") == 5.minutes)
      assert(ok[Duration](" 1.5 h ") == 90.minutes)
      assert(ok[Duration]("Inf") == Duration.Inf)
      assert(
        err[Duration]("soon") ==
          "expected a duration (e.g. '10 seconds', '5m' or 'Inf') for 'a.b', found 'soon'"
      )
      assert(err[Duration]("10").startsWith("expected a duration"))
    }
    test("finite duration") {
      import scala.concurrent.duration.*
      assert(ok[FiniteDuration]("100ms") == 100.millis)
      assert(
        err[FiniteDuration]("Inf") ==
          "expected a finite duration (e.g. '10 seconds' or '5m') for 'a.b', found 'Inf'"
      )
    }
    test("java duration") {
      assert(ok[java.time.Duration]("PT10S") == java.time.Duration.ofSeconds(10))
      assert(ok[java.time.Duration]("10 seconds") == java.time.Duration.ofSeconds(10))
      assert(ok[java.time.Duration]("100ms") == java.time.Duration.ofMillis(100))
      assert(
        err[java.time.Duration]("Inf") ==
          "expected a duration (e.g. '10 seconds' or 'PT10S') for 'a.b', found 'Inf'"
      )
    }
    test("java time") {
      import java.time.*
      assert(ok[Period]("P1Y2M3D") == Period.of(1, 2, 3))
      assert(ok[Instant]("2024-01-31T10:15:30Z") == Instant.ofEpochSecond(1706696130))
      assert(ok[LocalDate]("2024-01-31") == LocalDate.of(2024, 1, 31))
      assert(ok[LocalTime]("10:15") == LocalTime.of(10, 15))
      assert(ok[LocalDateTime]("2024-01-31T10:15:30") == LocalDateTime.of(2024, 1, 31, 10, 15, 30))
      assert(
        ok[OffsetDateTime]("2024-01-31T10:15:30+01:00") ==
          OffsetDateTime.of(2024, 1, 31, 10, 15, 30, 0, ZoneOffset.ofHours(1))
      )
      assert(
        ok[ZonedDateTime]("2024-01-31T10:15:30+01:00[Europe/Paris]") ==
          ZonedDateTime.of(2024, 1, 31, 10, 15, 30, 0, ZoneId.of("Europe/Paris"))
      )
      assert(ok[ZoneId]("Europe/Paris") == ZoneId.of("Europe/Paris"))
      assert(ok[ZoneId]("UTC") == ZoneId.of("UTC"))

      assert(err[Period]("1 year") == "expected a period (e.g. 'P1Y2M3D') for 'a.b', found '1 year'")
      assert(
        err[LocalDate]("2024-02-30") ==
          "expected a date (e.g. '2024-01-31') for 'a.b', found '2024-02-30'"
      )
      assert(err[LocalTime]("25:00").startsWith("expected a time"))
      assert(err[Instant]("2024-01-31").startsWith("expected an instant"))
      assert(err[LocalDateTime]("2024-01-31").startsWith("expected a date and time"))
      assert(err[OffsetDateTime]("2024-01-31T10:15:30").startsWith("expected a date and time with an offset"))
      assert(err[ZonedDateTime]("2024-01-31T10:15:30").startsWith("expected a date and time with a zone"))
      assert(
        err[ZoneId]("Mars/Olympus") ==
          "expected a time zone (e.g. 'Europe/Paris' or 'UTC') for 'a.b', found 'Mars/Olympus'"
      )
      assert(err[LocalDate](obj(file(1))()).endsWith("found an object"))
    }
    test("uuid") {
      val id = "123e4567-e89b-12d3-a456-426614174000"
      assert(ok[java.util.UUID](id) == java.util.UUID.fromString(id))
      assert(ok[java.util.UUID](id.toUpperCase) == java.util.UUID.fromString(id))
      assert(
        err[java.util.UUID]("1-2-3-4-5") ==
          "expected a UUID (e.g. '123e4567-e89b-12d3-a456-426614174000') for 'a.b', found '1-2-3-4-5'"
      )
      assert(err[java.util.UUID](id + "0").startsWith("expected a UUID"))
    }
    test("iterable") {
      import scala.collection.mutable
      def ints(xs: String*) = arr(file(1))(xs.map(str(_, file(1)))*)
      assert(read[List[Int]](ints("1", "2", "3"))._1 == Some(List(1, 2, 3)))
      assert(read[Vector[Int]](ints("1", "2"))._1 == Some(Vector(1, 2)))
      assert(read[Seq[Int]](ints("1", "2"))._1 == Some(Seq(1, 2)))
      assert(read[Set[Int]](ints("1", "2", "1"))._1 == Some(Set(1, 2)))
      assert(read[mutable.ArrayBuffer[Int]](ints("1"))._1 == Some(mutable.ArrayBuffer(1)))
      assert(read[List[Int]](ints())._1 == Some(Nil))
      assert(err[List[Int]]("1") == "expected an array for 'a.b', found '1'")
    }
    test("iterable errors") {
      val v = arr(file(1))(str("1", file(1)), str("x", file(1)), str("y", file(1)))
      val (result, out) = read[List[Int]](v)
      assert(result.isEmpty)
      // every element is read, so all errors are reported
      assert(
        out ==
          """error: app.conf:1:1: expected an integer for 'a.b.1', found 'x'
            |error: app.conf:1:1: expected an integer for 'a.b.2', found 'y'
            |""".stripMargin
      )
    }
    test("iterable from untyped string") {
      def split[A](raw: String, secret: Boolean = false)(using reader: Reader[A]): (Option[A], String) =
        val out = java.io.ByteArrayOutputStream()
        val value = Str(raw, LitKind.Unknown, List(env("APP_X")))
        if secret then value.markSecret()
        val result = reader.read(value, None, Context(Vector("a", "b"), Reporter.printing(java.io.PrintStream(out))))
        (result, out.toString)

      assert(split[List[String]]("a, b ,c")._1 == Some(List("a", "b", "c")))
      assert(split[List[String]]("a")._1 == Some(List("a")))
      assert(split[List[String]]("a,,b")._1 == Some(List("a", "", "b")))
      assert(split[List[String]]("a,")._1 == Some(List("a", "")))
      assert(split[List[String]]("")._1 == Some(Nil))
      assert(split[List[String]]("  ")._1 == Some(Nil))
      assert(split[Set[Int]]("1,2,1")._1 == Some(Set(1, 2)))

      val (result, out) = split[List[Int]]("1, x")
      assert(result.isEmpty)
      assert(out == "error: env APP_X: expected an integer for 'a.b.1', found 'x'\n")

      // items of a secret stay secret
      assert(
        split[List[Int]]("1, x", secret = true)._2 ==
          "error: env APP_X: expected an integer for 'a.b.1', found <secret>\n"
      )
    }
    test("iterable from typed string") {
      // typed formats have arrays, so a string there is a mistake
      assert(err[List[String]]("a,b") == "expected an array for 'a.b', found 'a,b'")
    }
    test("nested") {
      val v = arr(file(1))(
        arr(file(1))(str("1", file(1))),
        arr(file(1))(str("2", file(1)), str("x", file(2)))
      )
      val (result, out) = read[List[List[Int]]](v)
      assert(result.isEmpty)
      assert(out == "error: app.conf:2:1: expected an integer for 'a.b.1.1', found 'x'\n")
      val good = arr(file(1))(arr(file(1))(str("1", file(1))), arr(file(1))())
      assert(read[List[List[Int]]](good)._1 == Some(List(List(1), Nil)))
    }
    test("map") {
      import scala.collection.{SortedMap, mutable}
      val v = obj(file(1))("x" -> str("1", file(1)), "y" -> str("2", file(1)))
      assert(read[Map[String, Int]](v)._1 == Some(Map("x" -> 1, "y" -> 2)))
      assert(read[SortedMap[String, Int]](v)._1 == Some(SortedMap("x" -> 1, "y" -> 2)))
      assert(read[mutable.Map[String, Int]](v)._1 == Some(mutable.Map("x" -> 1, "y" -> 2)))
      assert(read[Map[String, Int]](obj(file(1))())._1 == Some(Map.empty))
      assert(err[Map[String, Int]](arr(file(1))()) == "expected an object for 'a.b', found an array")
    }
    test("map errors") {
      val v = obj(file(1))("x" -> str("x", file(1)), "y" -> str("2", file(1)), "z" -> str("z", file(1)))
      val (result, out) = read[Map[String, Int]](v)
      assert(result.isEmpty)
      assert(
        out ==
          """error: app.conf:1:1: expected an integer for 'a.b.x', found 'x'
            |error: app.conf:1:1: expected an integer for 'a.b.z', found 'z'
            |""".stripMargin
      )
    }
    test("map of lists") {
      val v = obj(file(1))(
        "x" -> arr(file(1))(str("1", file(1))),
        "y" -> arr(file(1))()
      )
      assert(read[Map[String, List[Int]]](v)._1 == Some(Map("x" -> List(1), "y" -> Nil)))
      val bad = obj(file(1))("x" -> arr(file(1))(str("1", file(1)), str("no", file(1))))
      assert(err[Map[String, List[Int]]](bad) == "expected an integer for 'a.b.x.1', found 'no'")
    }
    test("big numbers") {
      assert(ok[BigInt]("123456789012345678901234567890") == BigInt("123456789012345678901234567890"))
      assert(ok[BigInt](" -5 ") == BigInt(-5))
      assert(err[BigInt]("1.5") == "expected an integer for 'a.b', found '1.5'")
      assert(ok[BigDecimal]("1.25e-3") == BigDecimal("0.00125"))
      assert(err[BigDecimal]("NaN") == "expected a number for 'a.b', found 'NaN'")
    }
    test("uri") {
      assert(ok[java.net.URI]("https://example.com/a?b=c") == java.net.URI("https://example.com/a?b=c"))
      assert(ok[java.net.URI]("jdbc:postgresql://db:5432/app").getScheme == "jdbc")
      assert(err[java.net.URI]("a b").startsWith("expected a URI"))
    }
    test("socket address") {
      import java.net.InetSocketAddress
      def addr(raw: String) =
        val a = ok[InetSocketAddress](raw)
        (a.getHostString, a.getPort, a.isUnresolved)
      assert(addr("localhost:8080") == ("localhost", 8080, true))
      assert(addr(" 10.0.0.1:0 ") == ("10.0.0.1", 0, true))
      assert(addr("[::1]:443") == ("::1", 443, true))
      val expected = "expected a host and port (e.g. 'localhost:8080' or '[::1]:8080') for 'a.b', found"
      for bad <- List("localhost", ":80", "host:", "host:+80", "::1:80", "[::1]", "host:http") do
        assert(err[InetSocketAddress](bad) == s"$expected '$bad'")
      assert(
        err[InetSocketAddress]("host:70000") ==
          "expected a port between 0 and 65535 for 'a.b', found 'host:70000'"
      )
    }
    test("regex") {
      assert(ok[scala.util.matching.Regex]("a+b").matches("aaab"))
      assert(ok[java.util.regex.Pattern](" x ").pattern == " x ")
      val e = err[scala.util.matching.Regex]("(ab")
      assert(e.startsWith("expected a regular expression (Unclosed group"))
      assert(e.endsWith("for 'a.b', found '(ab'"))
      assert(err[java.util.regex.Pattern]("*").startsWith("expected a regular expression ("))
    }
    test("range") {
      assert(ok[Range]("1 to 10") == (1 to 10))
      assert(ok[Range]("0 until 10") == (0 until 10))
      assert(ok[Range](" 0 until 100 by 10 ") == (0 until 100 by 10))
      assert(ok[Range]("10 to 1 by -1") == (10 to 1 by -1))
      assert(ok[Range]("8000..8100") == (8000 to 8100))
      assert(ok[Range]("-5 .. 5 by 5") == (-5 to 5 by 5))
      assert(ok[Range]("5 until 5").isEmpty)
      val expected = "expected a range (e.g. '1 to 10', '0 until 10 by 2' or '1..10') for 'a.b', found"
      for bad <- List("1", "1 to", "1to10", "1 upto 10", "a to b", "1 to 10 by", "1...10") do
        assert(err[Range](bad) == s"$expected '$bad'")
      assert(err[Range]("1 to 10 by 0") == "expected a range with a non-zero step for 'a.b', found '1 to 10 by 0'")
      assert(err[Range]("1 to 9999999999").startsWith("expected a range with bounds and step between"))
      assert(
        err[Range]("-2147483648 to 2147483647") ==
          "expected a range with at most 2147483647 elements for 'a.b', found '-2147483648 to 2147483647'"
      )
    }
    test("option") {
      assert(ok[Option[Int]]("3") == Some(3))
      assert(read[Option[Int]](nul(file(1)))._1 == Some(None))
      assert(err[Option[Int]]("x") == "expected an integer for 'a.b', found 'x'")
      val v = arr(file(1))(str("1", file(1)), nul(file(1)))
      assert(read[List[Option[Int]]](v)._1 == Some(List(Some(1), None)))
    }
    test("array") {
      val v = arr(file(1))(str("1", file(1)), str("2", file(1)))
      assert(read[Array[Int]](v)._1.get.toList == List(1, 2))
      assert(read[Array[String]](arr(file(1))())._1.get.isEmpty)
      val bad = arr(file(1))(str("x", file(1)))
      assert(err[Array[Int]](bad) == "expected an integer for 'a.b.0', found 'x'")
    }
    test("paths") {
      def readPath(raw: String, origin: Origin): (Option[os.Path], String) =
        val out = java.io.ByteArrayOutputStream()
        val result = readers.OsPathReader.read(
          Str(raw, LitKind.String, List(origin)),
          None,
          Context(Vector("a", "b"), Reporter.printing(java.io.PrintStream(out)))
        )
        (result, out.toString)

      val conf = Origin.File("conf/app.yaml", 0, 1, 1)
      // relative to the config file
      assert(readPath("data/db", conf)._1 == Some(os.pwd / "conf" / "data" / "db"))
      assert(readPath("../logs", conf)._1 == Some(os.pwd / "logs"))
      // relative to the working directory, when not from a file
      assert(readPath("data", env("APP_DATA"))._1 == Some(os.pwd / "data"))
      assert(readPath("data", Origin.Props("app.data"))._1 == Some(os.pwd / "data"))
      // absolute and home paths
      assert(readPath("/var/lib/app", conf)._1 == Some(os.root / "var" / "lib" / "app"))
      assert(readPath("~", conf)._1 == Some(os.home))
      assert(readPath("~/.app", conf)._1 == Some(os.home / ".app"))
      // errors
      assert(readPath("", conf) == (None, "error: conf/app.yaml:1:1: expected a path for 'a.b', found ''\n"))

      // java paths are resolved in the same way
      val nio = readers.NioPathReader
        .read(Str("x", LitKind.String, List(conf)), None, Context(Reporter()))
      assert(nio.get == (os.pwd / "conf" / "x").toNIO)
    }
    test("overridden path root") {
      object etcReaders extends DefaultReaders:
        override def pathRoot(origin: Origin) = os.root / "etc" / "app"
      val result = etcReaders.OsPathReader.read(
        Str("certs/key.pem", LitKind.String, List(Origin.File("conf/app.yaml", 0, 1, 1))),
        None,
        Context(Reporter())
      )
      assert(result.get == os.root / "etc" / "app" / "certs" / "key.pem")
    }
    test("project") {
      val out = java.io.ByteArrayOutputStream()
      val reporter = Reporter.printing(java.io.PrintStream(out))
      assert(readers.project[Int](str("5", file(1)), reporter) == Some(5))
      assert(readers.project[Int](str("x", file(1)), reporter).isEmpty)
      assert(reporter.errors == 1)
    }
  }
