package example

// Every config file is parsed according to its extension.
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
  limits: Limits
) derives autoset.Reader

case class App(name: String, version: String) derives autoset.Reader
case class Server(host: String, ports: Seq[Int]) derives autoset.Reader
case class Db(url: java.net.URI, pool: Int) derives autoset.Reader
case class Logging(level: "debug" | "info" | "warn", file: os.Path) derives autoset.Reader
case class Limits(
  timeout: scala.concurrent.duration.Duration,
  retries: Int
) derives autoset.Reader

@main
def run() =
  val (config, raw) = autoset.read[Config](
    paths = Seq(
      os.pwd / "app.json",
      os.pwd / "server.yaml",
      os.pwd / "db.ini",
      os.pwd / "logging.conf",
      os.pwd / "limits.properties"
    )
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
  }
}
...
*/
