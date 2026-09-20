package autoset.derivation

import autoset.model.{Arr, Context, LitKind, Null, Obj, Origin, Reporter, Str, Value}
import scala.collection.mutable as m

trait ReadersApi:

  trait Reader[A]:

    /** Read a value and parse it as an `A`.
      *
      * On failure, report at least one error and return `None`. Readers may
      * mark parts of `value`, e.g. as secret.
      *
      * @param base
      *   values to fall back to for anything `value` leaves out, if any. This
      *   is ignored by readers of scalars and lists, since a value which is
      *   present replaces what it overrides entirely. Readers of objects fall
      *   back field by field, so that an object which only sets some of its
      *   keys inherits the rest from `base` instead of reporting them
      *   missing. It is where the defaults of a case class field come from,
      *   e.g. for `db: Db = Db("localhost")`, reading `db: {port: 1}` falls
      *   back to `Db("localhost")` for the host.
      */
    def read(value: Value, base: Option[A], ctx: Context): Option[A]

    /** Render `a` as the configuration it would be read from, or `None` if
      * this reader cannot render one.
      *
      * This is only used to show values which no source provided, so that the
      * configuration a reader read shows the defaults it used. It is not
      * parsed again, so it need not be an exact inverse of `read`; values it
      * cannot represent are shown as their `toString`.
      *
      * Origins are set by the caller, so implementations can use any.
      */
    def show(a: A): Option[Value] = None

  /** Companion of `Reader`, so that it can be used in `derives` clauses.
    * The `derived` method itself is added as an extension by
    * `DerivedReaders`.
    */
  object Reader

  /** A reader built from a `read` and a `show` function, which the derivation
    * macro generates.
    */
  def readerFrom[A](
      reader: (Value, Option[A], Context) => Option[A],
      shower: A => Option[Value]
  ): Reader[A] =
    new Reader[A]:
      def read(value: Value, base: Option[A], ctx: Context) = reader(value, base, ctx)
      override def show(a: A) = shower(a)

  /** Render `a` as configuration with `reader`, or as its `toString` if the
    * reader cannot render it. Origins are set by the caller.
    */
  def showValue[A](reader: Reader[A], a: A): Value =
    reader.show(a).getOrElse(Str(String.valueOf(a), LitKind.Unknown, Nil))

  /** Convert a config object into a scala type, emitting any error and/or
    * warnings.
    *
    * Readers fill in the values they defaulted to, and mark secrets and
    * unknown keys, in `obj`, so that it shows the configuration that was
    * used. See `Reader.read` for `base`.
    */
  def project[A](obj: Value, reporter: Reporter, base: Option[A] = None)(using
      reader: Reader[A]
  ): Option[A] =
    reader.read(obj, base, Context(reporter))

  /** Record in `obj` that its field at `key` took the value `a`, which no
    * source provided, so that the configuration shows what was used.
    *
    * The recorded value has the [[Origin.Default]] origin, which readers
    * treat as absent, so that reading `obj` again takes the default again
    * rather than parsing the recorded value.
    */
  def recordDefault[A](obj: Obj, key: String, reader: Reader[A], a: A, secret: Boolean): Unit =
    val value = ReaderUtils.asDefault(showValue(reader, a))
    if secret then value.markSecret()
    obj.fields(key) = value

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

  /** Report that the value being read is not what was expected. */
  def mismatch(expected: String, value: Value, ctx: Context): None.type =
    val at = if ctx.path.isEmpty then "" else s" for '${ctx.show}'"
    ctx.reporter.error(s"expected $expected$at, found ${describe(value)}", value.effectiveOrigin)
    None

  /** Whether `value` is one a reader recorded as a default, rather than one a
    * source provided. Readers treat these as absent, so that reading a
    * configuration again takes the same defaults instead of parsing what was
    * recorded, which is only meant to be shown.
    */
  def isDefault(value: Value): Boolean = value.origins == List(Origin.Default)

  /** A copy of `value` and everything in it with the [[Origin.Default]]
    * origin. This copies, so that recording what a reader rendered never
    * shares or modifies a value the reader holds on to.
    */
  def asDefault(value: Value): Value =
    val copy = value match
      case o: Obj =>
        Obj(o.fields.map((k, v) => k -> asDefault(v)).to(m.LinkedHashMap), List(Origin.Default))
      case a: Arr => Arr(a.values.map(asDefault).to(m.ListBuffer), List(Origin.Default))
      case s: Str => s.copy(origins = List(Origin.Default))
      case _: Null => Null(List(Origin.Default))
    copy.secret = value.secret
    copy

  /** Report that the field at `path` is required, but missing from `obj`.
    *
    * The error is reported where `obj` was declared, since that is where the
    * field could be added. An object which nothing declared, e.g. one which
    * only exists because an environment variable set something inside it, has
    * no such place, and then the error has no origin: a location which cannot
    * be acted on would be worse than none.
    */
  def missingField(obj: Obj, ctx: Context): None.type =
    val message = s"missing required field '${ctx.show}'"
    obj.declarationOrigin match
      case Some(origin) => ctx.reporter.error(message, origin)
      case None => ctx.reporter.error(message)
    None

  /** Look up the value of a field whose key is `name`, and which used to be
    * at the `deprecated` keys, in `obj`, returning the key it is at and its
    * value.
    *
    * A value at a deprecated key is warned about. If more than one key is
    * set, the first one of `name` and `deprecated` is used, and the others
    * are ignored with a warning, and marked as unknown so that they are not
    * shown. All the values are marked as secret if `secret`.
    */
  def lookupField(
      obj: Obj,
      name: String,
      deprecated: List[String],
      secret: Boolean,
      ctx: Context
  ): Option[(String, Value)] =
    def show(key: String) = (ctx / key).show
    val found = (name :: deprecated).flatMap(key => field(obj, key).map(key -> _))
    if secret then found.foreach((_, v) => v.markSecret())
    for (key, v) <- found.headOption if key != name do
      ctx.reporter.warn(
        s"key '${show(key)}' is deprecated, use '${show(name)}' instead",
        v.effectiveOrigin
      )
    for (key, v) <- found.drop(1) do
      v.unknown = true
      ctx.reporter.warn(
        s"key '${show(key)}' is deprecated, and ignored since '${show(found.head._1)}' is set",
        v.effectiveOrigin
      )
    found.headOption

  /** The value at `key` in `obj`, if a source provided one. Values which a
    * reader recorded as defaults are absent, see [[isDefault]].
    */
  def field(obj: Obj, key: String): Option[Value] = obj.fields.get(key).filterNot(isDefault)

  /** `ThisIsLowerCamelCase => thisIsLowerCamelCase`. A leading acronym is
    * lower-cased entirely: `HTTPServer => httpServer`, `URL => url`.
    */
  def lowerCamelCase(name: String): String =
    // the number of leading upper case letters
    val upper = name.takeWhile(_.isUpper).length
    // an upper case letter followed by a lower case one starts the next word
    val n =
      if upper > 1 && upper < name.length && name(upper).isLower then upper - 1
      else upper
    name.take(n).toLowerCase + name.drop(n)

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
