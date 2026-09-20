package example

// Config files describe the common case; a deployment usually needs to change
// a setting or two without editing them. Environment variables, system
// properties and command line arguments do that, and are applied in that
// order, after files and value directories. Arguments therefore win over
// everything else.

// `config.yaml`
// ```yaml
//include:../config.yaml
// ```

//snippet:start
case class Config(db: Db, server: Server) derives autoset.Reader

case class Db(host: String, port: Int, user: String) derives autoset.Reader
case class Server(port: Int, workers: Int) derives autoset.Reader

def main(args: Array[String]): Unit =
  // Arguments are independent of how a command line is parsed: translate the
  // settings which were actually given into `autoset.Arg`s. Here `k.v=x` is
  // taken as a setting, but a real app would get these from its command line
  // parser. Take care to only pass arguments which the user really gave: the
  // defaults of a command line parser would override all other config.
  val settings =
    for arg <- args.toSeq yield
      val Array(key, value) = arg.split("=", 2)
      autoset.Arg(name = arg, path = key.split("\\.").toList, value = value)

  val (config, raw) = autoset.read[Config](
    paths = Seq(os.pwd / "config.yaml"),

    // read every environment variable starting with this prefix. The rest of
    // the name is lower-cased and split on underscores, so `MYAPP_DB_HOST`
    // sets `db.host`
    envPrefix = "MYAPP_",

    // bind individual variables which don't follow the prefix convention
    envBinds = Seq("PGUSER" -> List("db", "user")),

    // the same for JVM system properties, whose names are already dotted, so
    // `-Dmyapp.server.workers=8` sets `server.workers`
    propsPrefix = "myapp.",
    propsBinds = Seq("http.proxyPort" -> List("server", "port")),

    args = settings
  ).getOrElse(sys.exit(1))

  println(raw.pretty())
//snippet:end

// With nothing set, the configuration is what the file says:

/* usage snippet
$ ./app
{ // config.yaml
  db: {
    host: "localhost",
    port: "5432",
    user: "example"
  },
  server: {
    port: "8080",
    workers: "4"
  }
}
*/

// Environment variables override files. Both the prefix and the explicit bind
// work the same way, and the origin names the variable a value came from:

/* usage snippet
$ MYAPP_DB_HOST=db.internal PGUSER=admin ./app
{ // config.yaml
  db: {
    host: "db.internal", // env MYAPP_DB_HOST (overrides config.yaml:2:9)
    port: "5432",
    user: "admin" // env PGUSER (overrides config.yaml:4:9)
  },
  server: {
    port: "8080",
    workers: "4"
  }
}
*/

// Since names are split on underscores, a variable cannot reach a key that
// contains one. Use `envBinds` for those, or pass an `envKeyReplacer` to spell
// the convention out, e.g. splitting on a double underscore instead.
//
// System properties come next. This example app is a launcher script which
// passes `JAVA_OPTS` on to the JVM:

/* usage snippet
$ JAVA_OPTS=-Dmyapp.server.workers=8 ./app
...
{ // config.yaml
  db: {
    host: "localhost",
    port: "5432",
    user: "example"
  },
  server: {
    port: "8080",
    workers: "8" // prop myapp.server.workers (overrides config.yaml:8:12)
  }
}
*/

// Arguments are applied last, so they override everything, including an
// environment variable setting the same key:

/* usage snippet
$ MYAPP_SERVER_PORT=9000 ./app db.host=127.0.0.1 server.port=9999
{ // config.yaml
  db: {
    host: "127.0.0.1", // arg db.host=127.0.0.1 (overrides config.yaml:2:9)
    port: "5432",
    user: "example"
  },
  server: {
    port: "9999", // arg server.port=9999 (overrides env MYAPP_SERVER_PORT, config.yaml:7:9)
    workers: "4"
  }
}
*/
