package example

// Every config file is parsed according to its extension. A file without an
// extension is parsed as INI, which is a reasonable default for the files
// found in `/etc`.
//
// This example spreads one configuration over one file per supported format,
// to show what each of them looks like.

// `app.json`
// ```json
//include:../app.json
// ```

// `server.yaml`
// ```yaml
//include:../server.yaml
// ```

// `db.ini`
// ```ini
//include:../db.ini
// ```

// `logging.conf` (HOCON)
// ```hocon
//include:../logging.conf
// ```

// `limits.properties`
// ```properties
//include:../limits.properties
// ```

// Formats which have no notion of types, such as INI, properties files and
// environment variables, are read as plain strings. Readers parse those
// strings, so `pool = 8` reads as an `Int` just like the JSON number `8`
// would.

//snippet:start
case class Config(
  app: App,
  server: Server,
  db: Db,
  logging: Logging,
  limits: Limits,
  features: Map[String, Boolean]
) derives autoset.Reader

case class App(name: String, version: String) derives autoset.Reader
case class Server(host: String, ports: Seq[Int]) derives autoset.Reader
case class Db(url: java.net.URI, pool: Int) derives autoset.Reader
case class Logging(level: "debug" | "info" | "warn", file: os.Path) derives autoset.Reader
case class Limits(
  timeout: scala.concurrent.duration.Duration,
  retries: Int
) derives autoset.Reader
//snippet:end

// ##### Defining your own format
//
// A format is a `autoset.FormatParser`, which turns the contents of a file
// into a configuration object. Values carry an `autoset.Origin`, so that
// errors can point back at the line they came from.
//
// Here is a parser for a minimal `key value` format, one pair per line:

// `features.rules`
// ```
//include:../features.rules
// ```

//snippet:start
object RulesParser extends autoset.FormatParser:
  def parse(
    name: String,
    stream: java.io.InputStream,
    sizeHint: Int,
    reporter: autoset.Reporter
  ): Option[autoset.Obj] =
    val root = autoset.Obj(
      collection.mutable.LinkedHashMap(),
      List(autoset.Origin.File(name, 0, 1, 1))
    )
    val text = String(stream.readAllBytes(), "utf-8")
    var ok = true

    for (line, idx) <- text.linesIterator.zipWithIndex if line.trim().nonEmpty do
      // origins point back at the source, so that errors can name the line a
      // value came from
      val origin = autoset.Origin.File(name, -1, idx + 1, 1)
      line.trim().split(" ", 2) match
        case _ if line.trim().startsWith("#") => // a comment
        case Array(key, value) =>
          // nest dotted keys into objects, so that `a.b value` sets `a.b`
          var obj = root
          val segments = key.split("\\.", -1).toList
          for seg <- segments.init do
            obj = obj.fields
              .getOrElseUpdate(
                seg,
                autoset.Obj(collection.mutable.LinkedHashMap(), List(origin))
              )
              .asInstanceOf[autoset.Obj]
          obj.fields(segments.last) =
            autoset.Str(value.trim(), autoset.LitKind.Unknown, List(origin))
        case _ =>
          // a parser reports problems instead of throwing, and returns `None`
          // if it reported an error
          reporter.error("expected 'key value'", origin, line)
          ok = false

    Option.when(ok)(root)
//snippet:end

// Pass it in the `parsers` map, keyed by the file extension it handles. Adding
// to `autoset.defaultParsers` keeps the built-in formats available:

//snippet:start
@main
def run() =
  val (config, raw) = autoset.read[Config](
    paths = Seq(
      os.pwd / "app.json",
      os.pwd / "server.yaml",
      os.pwd / "db.ini",
      os.pwd / "logging.conf",
      os.pwd / "limits.properties",
      os.pwd / "features.rules"
    ),
    parsers = autoset.defaultParsers + ("rules" -> RulesParser)
  ).getOrElse(sys.exit(1))

  println(raw.pretty())
  println(config)
//snippet:end

// Regardless of the format they were written in, all values end up in the same
// configuration:

/* usage snippet
$ ./app
{ // app.json
  app: {
    name: "example",
    version: "1.0.0"
  },
  server: { // server.yaml
    host: "localhost",
    ports: ["80", "443"]
  },
  db: { // db.ini
    url: "jdbc:postgresql://localhost/example",
    pool: "8"
  },
  logging: { // logging.conf
    level: "debug",
    file: "/var/log/example.log"
  },
  limits: { // limits.properties
    timeout: "30 seconds",
    retries: "3"
  },
  features: { // features.rules
    beta: "true",
    tracing: "false"
  }
}
...
*/
