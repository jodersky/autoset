package example

// A `autoset.Reader[A]` turns a config value into an `A`. Readers for
// primitives, for the standard library types that show up in configuration,
// and for collections are all available out of the box.

// `config.yaml`
// ```yaml
//include:../config.yaml
// ```

//snippet:start
case class Config(
  // primitives: String, Boolean, Char, Byte, Short, Int, Long, Float, Double
  name: String,
  port: Int,
  ratio: Double,
  debug: Boolean,

  // and common standard library types
  id: java.util.UUID,
  url: java.net.URI,
  addr: java.net.InetSocketAddress, // 'host:port', never resolved
  timeout: scala.concurrent.duration.Duration, // '30 seconds', '5m', 'Inf'
  retention: java.time.Duration, // ISO-8601 'P30D', or '30 days'
  startsAt: java.time.LocalTime, // also Instant, LocalDate, ZonedDateTime, ...
  zone: java.time.ZoneId,
  logFile: os.Path, // also java.nio.file.Path
  pattern: scala.util.matching.Regex,
  workers: Range, // '1 to 8', '0 until 8 by 2'

  // binary data, from a base64-encoded string
  apiKey: Array[Byte],
  license: geny.Readable, // the same, as a source which can be read repeatedly

  // any collection with a `Factory`: Seq, List, Vector, Set, mutable.Buffer...
  hosts: Seq[String],
  tags: Set[String],

  // any map with string keys
  weights: Map[String, Double],

  // `Option` is read from `null`. Note that a field is optional because it has
  // a default, not because it is an `Option`
  motd: Option[String] = None
) derives autoset.Reader

@main
def run() =
  val (config, _) = autoset.read[Config](
    paths = Seq(os.pwd / "config.yaml"),
    envPrefix = "MYAPP_"
  ).getOrElse(sys.exit(1))

  println(s"port      = ${config.port}")
  println(s"debug     = ${config.debug}")
  println(s"url       = ${config.url}")
  println(s"addr      = ${config.addr}")
  println(s"timeout   = ${config.timeout}")
  println(s"retention = ${config.retention}")
  println(s"startsAt  = ${config.startsAt}")
  println(s"logFile   = ${config.logFile.relativeTo(os.pwd)}")
  println(s"workers   = ${config.workers}")
  println(s"apiKey    = ${config.apiKey.length} bytes")
  println(s"license   = ${String(config.license.readBytesThrough(_.readAllBytes()))}")
  println(s"hosts     = ${config.hosts}")
  println(s"tags      = ${config.tags}")
  println(s"weights   = ${config.weights}")
  println(s"motd      = ${config.motd}")
//snippet:end

// Relative paths are resolved against the directory of the file they were read
// from, so that a config file can refer to its neighbours no matter what the
// working directory of the process is. A path from anywhere else, such as an
// environment variable, is relative to the working directory instead. Override
// `pathRoot` to change that.

/* usage snippet
$ ./app
port      = 8080
debug     = true
url       = https://example.com/api
addr      = localhost/<unresolved>:9090
timeout   = 30 seconds
retention = PT720H
startsAt  = 04:30
logFile   = log/example.log
workers   = Range 1 to 8
apiKey    = 12 bytes
license   = MIT
hosts     = List(a.example.com, b.example.com)
tags      = Set(red, green)
weights   = Map(a -> 0.5, b -> 1.5)
motd      = None
*/

// Formats which have no types of their own — INI and properties files,
// environment variables, system properties and arguments — supply plain
// strings. A collection reads such a string by splitting it on commas, so that
// a list can be set from a single environment variable:

/* usage snippet
$ MYAPP_HOSTS="c.example.com, d.example.com" ./app
...
hosts     = List(c.example.com, d.example.com)
...
*/
