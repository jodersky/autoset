package example

// Readers for your own types are derived, either in a `derives` clause or
// explicitly with `autoset.readerFor[A]`. Three shapes are supported: case
// classes, sealed types (including enums), and unions of string literals.

// `config.yaml`
// ```yaml
//include:../config.yaml
// ```

// ##### Case classes
//
// A case class is read from an object, field by field. A field with a default
// may be left out; a field without one is required, and missing it is an
// error. Keys which match no field are warned about.
//
// Note that an `Option` field is not optional by virtue of being an `Option`:
// it may be set to `null`, which reads as `None`, but it must still be set
// unless it has a default.

//snippet:start
case class Server(
  host: String,
  port: Int = 8080, // optional, because it has a default

  // the key in config files, when it differs from the Scala field name
  @autoset.name("db_url") dbUrl: java.net.URI,

  // still accepted, with a warning pointing at the new name
  @autoset.deprecatedNames("motd") banner: Option[String] = None,

  // never shown, neither by `pretty()` nor in error messages
  @autoset.secret password: Option[String] = None
) derives autoset.Reader
//snippet:end

// ##### Sealed types and enums
//
// A sealed trait or enum is read from an object with a `type` key naming the
// case, and the rest of the object is read as that case. Nested sealed types
// are flattened to their cases. A case without parameters may be written as
// just its name, so `cache: memory` is short for `cache: {type: memory}`.
//
// Case names are lowerCamelCase by default, so `InMemory` is written
// `inMemory`; both that and the `type` key itself are configurable.

//snippet:start
sealed trait Storage derives autoset.Reader
case class Disk(path: os.Path, sync: Boolean = false) extends Storage
case class S3(bucket: String, @autoset.secret key: String) extends Storage

enum Cache derives autoset.Reader:
  case Memory // `cache: memory`
  case Redis(addr: java.net.InetSocketAddress) // `cache: {type: redis, addr: ...}`

enum Level derives autoset.Reader:
  case Debug, Info, Warn
//snippet:end

// ##### Unions of string literals
//
// A union of string literal types reads as one of those strings. This needs no
// declaration of its own: unlike the other two shapes, a field written as a
// union gets its reader derived on the spot.

//snippet:start
case class Config(
  server: Server,
  storage: Storage,
  cache: Cache,
  level: Level,
  mode: "strict" | "lenient"
) derives autoset.Reader

@main
def run() =
  val (config, _) = autoset.read[Config](
    paths = Seq(os.pwd / "config.yaml"),
    envPrefix = "MYAPP_"
  ).getOrElse(sys.exit(1))

  println(config)
//snippet:end

/* usage snippet
$ ./app
Config(Server(0.0.0.0,8080,jdbc:postgresql://localhost/example,None,None),Disk(/var/lib/example,false),Memory,Info,strict)
*/

// An object which is only partly set inherits the rest from what it would
// otherwise have defaulted to, so setting one key of `server` does not make
// the others missing:

/* usage snippet
$ MYAPP_SERVER_PORT=9090 ./app
Config(Server(0.0.0.0,9090,jdbc:postgresql://localhost/example,None,None),Disk(/var/lib/example,false),Memory,Info,strict)
*/

// Selecting another case of a sealed type replaces it wholesale, since the
// fields of the two cases have nothing to do with each other. The keys of the
// case which was not selected are then simply unknown:

/* usage snippet
$ MYAPP_STORAGE_TYPE=s3 MYAPP_STORAGE_BUCKET=example MYAPP_STORAGE_KEY=s3cr3t ./app
warning: config.yaml:7:9: unknown key 'storage.path'
Config(Server(0.0.0.0,8080,jdbc:postgresql://localhost/example,None,None),S3(example,s3cr3t),Memory,Info,strict)
*/

// A deprecated key still works, and says what to write instead:

/* usage snippet
$ MYAPP_SERVER_MOTD=hello ./app
warning: env MYAPP_SERVER_MOTD: key 'server.motd' is deprecated, use 'server.banner' instead
Config(Server(0.0.0.0,8080,jdbc:postgresql://localhost/example,Some(hello),None),Disk(/var/lib/example,false),Memory,Info,strict)
*/
