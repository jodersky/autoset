package example

// Sooner or later a config needs a type that autoset has never heard of. A
// reader is a small trait with one required method, so writing one by hand is
// not much work.

// `config.yaml`
// ```yaml
//include:../config.yaml
// ```

// Take a byte size, written the way people actually write byte sizes:

//snippet:start
case class ByteSize(bytes: Long)

object ByteSize:
  private val units = Map(
    "" -> 1L, "B" -> 1L,
    "kB" -> 1000L, "MB" -> 1000_000L, "GB" -> 1000_000_000L,
    "KiB" -> 1024L, "MiB" -> 1048576L, "GiB" -> 1073741824L
  )
  private val syntax = raw"(\d+)\s*(${units.keys.mkString("|")})".r

  def parse(s: String): Option[ByteSize] = s.trim match
    case syntax(number, unit) => number.toLongOption.map(n => ByteSize(n * units(unit)))
    case _ => None
//snippet:end

// A reader turns a config value into an `A`, or reports at least one error and
// returns `None`. Give it a `given` for the type it reads, and every field of
// that type, anywhere in the config, is read with it.

//snippet:start
given autoset.Reader[ByteSize] with
  private val expected = "a size (e.g. '512', '4kB' or '1 GiB')"

  def read(
    value: autoset.Value,
    base: Option[ByteSize], // what this value falls back to, see below
    ctx: autoset.model.Context // where in the config we are, and where to report
  ): Option[ByteSize] =
    value match
      case autoset.Str(raw, _, _) =>
        ByteSize.parse(raw) match
          case Some(size) => Some(size)
          // reports "expected a size ..., found '...'" at the value's origin
          case None => autoset.ReaderUtils.mismatch(expected, value, ctx)
      case _ => autoset.ReaderUtils.mismatch(expected, value, ctx)

  // optional: render a value back as config, so that a default which no file
  // set can be shown in the merged configuration. Origins are filled in by the
  // caller, so `Nil` here
  override def show(a: ByteSize) =
    Some(autoset.Str(s"${a.bytes}B", autoset.LitKind.String, Nil))
//snippet:end

// `base` is what the value falls back to for anything it leaves out. Readers
// of scalars ignore it, since a value which is present replaces what it
// overrides entirely; readers of objects fall back key by key, which is how a
// partly-set object inherits the rest of its fields.
//
// To read one field differently from the rest of its type, name a reader with
// `@autoset.readWith` instead of putting it in scope. The reader has to be a
// stable reference, such as a member of an object:

//snippet:start
object Hex:
  val int: autoset.Reader[Int] = new autoset.Reader[Int]:
    def read(value: autoset.Value, base: Option[Int], ctx: autoset.model.Context) =
      value match
        case autoset.Str(raw, _, _) if raw.trim.startsWith("0x") =>
          scala.util.Try(Integer.parseInt(raw.trim.drop(2), 16)).toOption match
            case Some(i) => Some(i)
            case None => autoset.ReaderUtils.mismatch("a hex number", value, ctx)
        case _ => autoset.ReaderUtils.mismatch("a hex number (e.g. '0xff')", value, ctx)

    override def show(a: Int) = Some(autoset.Str(f"0x$a%06x", autoset.LitKind.String, Nil))

case class Theme(@autoset.readWith(Hex.int) background: Int) derives autoset.Reader
//snippet:end

//snippet:start
case class Config(cache: Cache, theme: Theme) derives autoset.Reader

case class Cache(
  size: ByteSize,
  entry: ByteSize = ByteSize(64 * 1024)
) derives autoset.Reader

@main
def run() =
  val (config, raw) = autoset.read[Config](
    paths = Seq(os.pwd / "config.yaml"),
    envPrefix = "MYAPP_"
  ).getOrElse(sys.exit(1))

  println(raw.pretty())
  println(config)
//snippet:end

// `entry` was set by no file, so it is shown as what `show` rendered:

/* usage snippet
$ ./app
{ // config.yaml
  cache: {
    size: "512MB",
    entry: "65536B" // default
  },
  theme: {
    background: "0x1e1e2e"
  }
}
Config(Cache(ByteSize(512000000),ByteSize(65536)),Theme(1973806))
*/

// And an unparseable value is reported where it was written, in the words the
// reader chose:

/* usage snippet
$ MYAPP_CACHE_SIZE=lots ./app
error: env MYAPP_CACHE_SIZE: expected a size (e.g. '512', '4kB' or '1 GiB') for 'cache.size', found 'lots'
*/
