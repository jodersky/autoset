package autoset.derivation

import autoset.model.*
import ReaderUtils.mismatch

/** Readers for the config model's values, primitives and common standard
  * library types.
  */
trait BaseReaders extends ReadersApi:

  given ValueReader: Reader[Value] with
    def read(value: Value, base: Option[Value], ctx: Context) = Some(value)
    override def show(a: Value) = Some(a)

  given ObjReader: Reader[Obj] with
    override def show(a: Obj) = Some(a)
    def read(value: Value, base: Option[Obj], ctx: Context) =
      value match
        case o: Obj => Some(o)
        case _ => mismatch("an object", value, ctx)

  given ArrReader: Reader[Arr] with
    override def show(a: Arr) = Some(a)
    def read(value: Value, base: Option[Arr], ctx: Context) =
      value match
        case a: Arr => Some(a)
        case _ => mismatch("an array", value, ctx)

  given StrReader: Reader[Str] with
    override def show(a: Str) = Some(a)
    def read(value: Value, base: Option[Str], ctx: Context) =
      value match
        case s: Str => Some(s)
        case _ => mismatch("a string", value, ctx)

  given NullReader: Reader[Null] with
    override def show(a: Null) = Some(a)
    def read(value: Value, base: Option[Null], ctx: Context) =
      value match
        case n: Null => Some(n)
        case _ => mismatch("null", value, ctx)

  given StringReader: Reader[String] with
    override def show(a: String) = Some(Str(a, LitKind.String, Nil))
    def read(value: Value, base: Option[String], ctx: Context) =
      value match
        case Str(raw, _, _) => Some(raw)
        case _ => mismatch("a string", value, ctx)

  /** A reader for strings that parse as `A`. `parse` returns `Left` with a
    * description of what was expected if the string is invalid.
    */
  protected class ParsedReader[A](expected: String, kind: LitKind = LitKind.String)(
      parse: String => Either[String, A]
  ) extends Reader[A]:
    override def show(a: A) = Some(Str(String.valueOf(a), kind, Nil))
    def read(value: Value, base: Option[A], ctx: Context) =
      value match
        case Str(raw, _, _) =>
          parse(raw) match
            case Right(a) => Some(a)
            case Left(exp) => mismatch(exp, value, ctx)
        case _ => mismatch(expected, value, ctx)

  /** A reader for integral numbers, reporting when a number is out of range. */
  protected def integral[A](min: A, max: A)(parse: String => Option[A]): Reader[A] =
    ParsedReader("an integer", LitKind.Num) { raw =>
      val s = raw.trim
      parse(s) match
        case Some(a) => Right(a)
        case None if s.matches("[+-]?[0-9]+") => Left(s"an integer between $min and $max")
        case None => Left("an integer")
    }

  /** A reader for floating point numbers, reporting when a number overflows. */
  protected def fractional[A](parse: String => Option[A])(isInfinite: A => Boolean): Reader[A] =
    ParsedReader("a number", LitKind.Num) { raw =>
      val s = raw.trim
      // Java accepts type suffixes such as `1d` or `1f`, which aren't numbers in config files
      val suffixed = s.nonEmpty && "dDfF".contains(s.last)
      parse(s).filter(_ => !suffixed) match
        case Some(a) if isInfinite(a) && !s.toLowerCase.contains("infinity") =>
          Left("a number within range")
        case Some(a) => Right(a)
        case None => Left("a number")
    }

  given BooleanReader: Reader[Boolean] = ParsedReader("a boolean", LitKind.Bool) { raw =>
    raw.trim.toLowerCase match
      case "true" => Right(true)
      case "false" => Right(false)
      case _ => Left("a boolean ('true' or 'false')")
  }

  given CharReader: Reader[Char] = ParsedReader("a single character") { raw =>
    if raw.length == 1 then Right(raw.head) else Left("a single character")
  }

  given ByteReader: Reader[Byte] = integral(Byte.MinValue, Byte.MaxValue)(_.toByteOption)
  given ShortReader: Reader[Short] = integral(Short.MinValue, Short.MaxValue)(_.toShortOption)
  given IntReader: Reader[Int] = integral(Int.MinValue, Int.MaxValue)(_.toIntOption)
  given LongReader: Reader[Long] = integral(Long.MinValue, Long.MaxValue)(_.toLongOption)

  given FloatReader: Reader[Float] = fractional(_.toFloatOption)(_.isInfinite)
  given DoubleReader: Reader[Double] = fractional(_.toDoubleOption)(_.isInfinite)

  /** A reader for strings that `parse` accepts, where `parse` throws on
    * invalid input.
    */
  protected def parsing[A](expected: String, kind: LitKind = LitKind.String)(
      parse: String => A
  ): Reader[A] =
    ParsedReader(expected, kind) { raw =>
      try Right(parse(raw.trim))
      catch case scala.util.control.NonFatal(_) => Left(expected)
    }

  given DurationReader: Reader[scala.concurrent.duration.Duration] =
    parsing("a duration (e.g. '10 seconds', '5m' or 'Inf')")(
      scala.concurrent.duration.Duration(_)
    )

  private def finiteDuration(s: String): Option[scala.concurrent.duration.FiniteDuration] =
    try
      scala.concurrent.duration.Duration(s) match
        case d: scala.concurrent.duration.FiniteDuration => Some(d)
        case _ => None
    catch case scala.util.control.NonFatal(_) => None

  given FiniteDurationReader: Reader[scala.concurrent.duration.FiniteDuration] =
    val expected = "a finite duration (e.g. '10 seconds' or '5m')"
    ParsedReader(expected)(raw => finiteDuration(raw.trim).toRight(expected))

  /** Accepts both ISO-8601 durations and the Scala duration syntax. */
  given JavaDurationReader: Reader[java.time.Duration] =
    val expected = "a duration (e.g. '10 seconds' or 'PT10S')"
    ParsedReader(expected) { raw =>
      val s = raw.trim
      try Right(java.time.Duration.parse(s))
      catch
        case scala.util.control.NonFatal(_) =>
          finiteDuration(s).map(d => java.time.Duration.ofNanos(d.toNanos)).toRight(expected)
    }

  given PeriodReader: Reader[java.time.Period] =
    parsing("a period (e.g. 'P1Y2M3D')")(java.time.Period.parse(_))

  given InstantReader: Reader[java.time.Instant] =
    parsing("an instant (e.g. '2024-01-31T10:15:30Z')")(java.time.Instant.parse(_))

  given LocalDateReader: Reader[java.time.LocalDate] =
    parsing("a date (e.g. '2024-01-31')")(java.time.LocalDate.parse(_))

  given LocalTimeReader: Reader[java.time.LocalTime] =
    parsing("a time (e.g. '10:15:30')")(java.time.LocalTime.parse(_))

  given LocalDateTimeReader: Reader[java.time.LocalDateTime] =
    parsing("a date and time (e.g. '2024-01-31T10:15:30')")(java.time.LocalDateTime.parse(_))

  given OffsetDateTimeReader: Reader[java.time.OffsetDateTime] =
    parsing("a date and time with an offset (e.g. '2024-01-31T10:15:30+01:00')")(
      java.time.OffsetDateTime.parse(_)
    )

  given ZonedDateTimeReader: Reader[java.time.ZonedDateTime] =
    parsing("a date and time with a zone (e.g. '2024-01-31T10:15:30+01:00[Europe/Paris]')")(
      java.time.ZonedDateTime.parse(_)
    )

  given ZoneIdReader: Reader[java.time.ZoneId] =
    parsing("a time zone (e.g. 'Europe/Paris' or 'UTC')")(java.time.ZoneId.of(_))

  given UUIDReader: Reader[java.util.UUID] =
    val expected = "a UUID (e.g. '123e4567-e89b-12d3-a456-426614174000')"
    // `UUID.fromString` also accepts non-canonical forms such as `1-2-3-4-5`
    val canonical = "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"
    ParsedReader(expected) { raw =>
      val s = raw.trim
      if s.matches(canonical) then Right(java.util.UUID.fromString(s)) else Left(expected)
    }

  given BigIntReader: Reader[BigInt] = parsing("an integer", LitKind.Num)(BigInt(_))

  given BigDecimalReader: Reader[BigDecimal] = parsing("a number", LitKind.Num)(BigDecimal(_))

  given URIReader: Reader[java.net.URI] =
    parsing("a URI (e.g. 'https://example.com/path')")(java.net.URI(_))

  /** Reads `host:port`, with IPv6 hosts in brackets. The host is not resolved,
    * so reading a config never does DNS lookups.
    */
  given InetSocketAddressReader: Reader[java.net.InetSocketAddress] =
    val expected = "a host and port (e.g. 'localhost:8080' or '[::1]:8080')"
    ParsedReader(expected) { raw =>
      val s = raw.trim
      val (host, port) =
        if s.startsWith("[") then
          s.indexOf("]:") match
            case -1 => ("", "")
            case end => (s.substring(1, end), s.substring(end + 2))
        else
          s.lastIndexOf(':') match
            // more than one colon is an IPv6 address without brackets
            case i if i < 0 || s.indexOf(':') != i => ("", "")
            case i => (s.take(i), s.drop(i + 1))
      if host.isEmpty || port.isEmpty || !port.forall(_.isDigit) then Left(expected)
      else
        port.toIntOption.filter(_ <= 65535) match
          case Some(p) => Right(java.net.InetSocketAddress.createUnresolved(host, p))
          case None => Left("a port between 0 and 65535")
    }

  /** Reads `1 to 10`, `0 until 10` or `1..10` (the same as `1 to 10`), each
    * optionally followed by a step, e.g. `0 until 100 by 10`.
    */
  given RangeReader: Reader[Range] =
    val expected = "a range (e.g. '1 to 10', '0 until 10 by 2' or '1..10')"
    val syntax = """([+-]?\d+)(?:\s*\.\.\s*|\s+(to|until)\s+)([+-]?\d+)(?:\s+by\s+([+-]?\d+))?""".r
    ParsedReader(expected) { raw =>
      raw.trim match
        case syntax(start, kind, end, step) =>
          (start.toIntOption, end.toIntOption, Option(step).fold(Some(1))(_.toIntOption)) match
            case (Some(_), Some(_), Some(0)) => Left("a range with a non-zero step")
            case (Some(a), Some(b), Some(by)) =>
              val range = if kind == "until" then Range(a, b, by) else Range.inclusive(a, b, by)
              // a range with more than `Int.MaxValue` elements only fails when
              // its length is computed, so check here rather than in user code
              try
                range.length
                Right(range)
              catch
                case _: IllegalArgumentException =>
                  Left(s"a range with at most ${Int.MaxValue} elements")
            case _ => Left(s"a range with bounds and step between ${Int.MinValue} and ${Int.MaxValue}")
        case _ => Left(expected)
    }

  /** Compile a regular expression, describing what is wrong with it if invalid. */
  private def compileRegex(raw: String): Either[String, java.util.regex.Pattern] =
    try Right(java.util.regex.Pattern.compile(raw))
    catch
      case e: java.util.regex.PatternSyntaxException =>
        Left(s"a regular expression (${e.getDescription})")

  given PatternReader: Reader[java.util.regex.Pattern] =
    ParsedReader("a regular expression")(compileRegex)

  given RegexReader: Reader[scala.util.matching.Regex] =
    ParsedReader("a regular expression")(raw => compileRegex(raw).map(_ => raw.r))

  /** The directory against which a relative path read from `origin` is
    * resolved.
    *
    * By default, a path from a config file is relative to the directory of
    * that file, and any other path (e.g. from an environment variable) is
    * relative to the working directory. Override this to resolve paths
    * against other directories, for example:
    *
    * ```scala
    * override def pathRoot(origin: Origin) = os.Path("/etc/myapp")
    * ```
    */
  def pathRoot(origin: Origin): os.Path = origin match
    // files not loaded with `Api.load` may only have a relative name
    case Origin.File(file, _, _, _, absolute) => os.Path(absolute.getOrElse(file), os.pwd) / os.up
    case _ => os.pwd

  /** Reads a path, resolving relative paths with `pathRoot`, and `~` as the
    * home directory.
    */
  given OsPathReader: Reader[os.Path] with
    override def show(a: os.Path) = Some(Str(a.toString, LitKind.String, Nil))
    def read(value: Value, base: Option[os.Path], ctx: Context) =
      value match
        case Str(raw, _, _) =>
          val resolved =
            try
              if raw.isEmpty then None
              else if raw == "~" then Some(os.home)
              else if raw.startsWith("~/") then Some(os.home / os.RelPath(raw.drop(2)))
              else Some(os.Path(raw, pathRoot(value.effectiveOrigin)))
            catch case scala.util.control.NonFatal(_) => None
          resolved match
            case Some(p) => Some(p)
            case None => mismatch("a path", value, ctx)
        case _ => mismatch("a path", value, ctx)

  given NioPathReader: Reader[java.nio.file.Path] with
    override def show(a: java.nio.file.Path) = Some(Str(a.toString, LitKind.String, Nil))
    def read(value: Value, base: Option[java.nio.file.Path], ctx: Context) =
      OsPathReader.read(value, None, ctx).map(_.toNIO)

  private val expectedBase64 = "base64-encoded data"

  /** Decode base64 data, accepting both the standard and URL-safe alphabets,
    * with or without padding. Whitespace is ignored, so that data may be
    * wrapped over several lines.
    */
  private def decodeBase64(raw: String): Either[String, Array[Byte]] =
    val s = raw.filterNot(_.isWhitespace)
    val decoder =
      if s.contains('-') || s.contains('_') then java.util.Base64.getUrlDecoder
      else java.util.Base64.getDecoder
    try Right(decoder.decode(s))
    catch case _: IllegalArgumentException => Left(expectedBase64)

  /** Reads binary data, such as a key or a certificate, from a base64-encoded
    * string (see `decodeBase64` for what is accepted).
    */
  given ByteArrayReader: Reader[Array[Byte]] =
    new ParsedReader[Array[Byte]](expectedBase64)(decodeBase64):
      override def show(a: Array[Byte]) =
        Some(Str(java.util.Base64.getEncoder.encodeToString(a), LitKind.String, Nil))

  /** Reads binary data from a base64-encoded string, as a source which can be
    * read as many times as needed (see `ByteArrayReader`).
    */
  given ReadableReader: Reader[geny.Readable] with
    // showing a `Readable` reads it, which consumes it if it is backed by a
    // stream. Only defaults are shown, and those are values a program built
    // itself, which are backed by bytes or a string in practice.
    override def show(a: geny.Readable) =
      val out = java.io.ByteArrayOutputStream()
      try
        a.writeBytesTo(out)
        ByteArrayReader.show(out.toByteArray)
      catch case scala.util.control.NonFatal(_) => None
    def read(value: Value, base: Option[geny.Readable], ctx: Context) =
      ByteArrayReader.read(value, None, ctx).map(geny.Readable.ByteArrayReadable(_))
