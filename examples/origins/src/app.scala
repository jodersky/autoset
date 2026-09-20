package example

// "Where did that setting come from?" is one of the more annoying questions to
// answer about a running service, once config is spread over a file, a
// drop-in directory, a secret volume and a handful of environment variables.
//
// Every value keeps the origin it was read from, and the origins of the values
// it replaced. `autoset.read` returns the merged configuration alongside the
// parsed one, and rendering it shows what the application actually saw, and
// where each part of it came from.

// `config.yaml`
// ```yaml
//include:../config.yaml
// ```

// `secrets/db.password`
// ```
//include:../secrets/db.password
// ```

//snippet:start
case class Config(db: Db, logging: Logging) derives autoset.Reader

case class Db(
  host: String,
  port: Int,
  @autoset.secret password: String,
  pool: Int = 8
) derives autoset.Reader

case class Logging(
  level: String,
  target: String,
  format: String = "text"
) derives autoset.Reader

def main(args: Array[String]): Unit =
  val (config, raw) = autoset.read[Config](
    paths = Seq(os.pwd / "config.yaml"),
    valueDirs = Seq(os.pwd / "secrets"),
    envPrefix = "MYAPP_"
  ).getOrElse(sys.exit(1))

  println(raw.pretty(verbose = args.contains("--verbose")))
//snippet:end

// Note the annotations: a container is labelled with the source most of its
// contents come from, and anything that disagrees with that label carries its
// own. Values which no source set are filled in with the default of their
// field, and marked as such, so that the rendering is the whole configuration
// rather than only the parts someone wrote down.
//
// Values of fields annotated with `@autoset.secret` are shown as `<secret>`,
// here the password picked up from the secret volume. Their origin is still
// shown: it is the value that must not leak, not the fact that it was set.

/* usage snippet
$ MYAPP_DB_POOL=16 MYAPP_LOGGING_LEVEL=debug ./app
{ // config.yaml
  db: {
    host: "localhost",
    port: "5432",
    password: <secret>, // secrets/db.password
    pool: "16" // env MYAPP_DB_POOL
  },
  logging: {
    level: "debug", // env MYAPP_LOGGING_LEVEL (overrides config.yaml:6:10)
    target: "/var/log/example.log",
    format: "text" // default
  }
}
*/

// `pretty(verbose = true)` annotates every value with all of its origins
// instead, which is what you want when a value is not what you expect and the
// summary is hiding the source you care about:

/* usage snippet
$ MYAPP_DB_POOL=16 MYAPP_LOGGING_LEVEL=debug ./app --verbose
{ // env MYAPP_LOGGING_LEVEL, env MYAPP_DB_POOL, secrets/db.password, config.yaml:1:1, default
  db: { // env MYAPP_DB_POOL, secrets/db.password, config.yaml:2:3
    host: "localhost", // config.yaml:2:9
    port: "5432", // config.yaml:3:9
    password: <secret>, // secrets/db.password
    pool: "16" // env MYAPP_DB_POOL
  },
  logging: { // env MYAPP_LOGGING_LEVEL, config.yaml:6:3
    level: "debug", // env MYAPP_LOGGING_LEVEL (overrides config.yaml:6:10)
    target: "/var/log/example.log", // config.yaml:7:11
    format: "text" // default
  }
}
*/

// Since a configuration object is only ever rendered this way — its `toString`
// calls `pretty()` — there is no way to log one and leak a secret by accident.
