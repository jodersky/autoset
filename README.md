# Autoset

Read configuration from multiple formats, and map it to scala types.

## Example


1. Define your config model in Scala


```scala
case class AppConfig(
  main: ServerConfig,
  metrics: ServerConfig,
  db: DatabaseConfig,
  root: os.Path
) derives autoset.Reader

case class ServerConfig(
  listeners: Seq[Listener],
  motd: Option[String] = None,
) derives autoset.Reader

case class Listener(
  scheme: "http" | "https",
  port: Int
) derives autoset.Reader

enum DatabaseConfig derives autoset.Reader:
  case Sqlite(file: os.Path)
  case Postgres(
    uri: java.net.URI,
    user: String = "postgres",
    @autoset.secret password: String
  )
```

2. Your config file (can be YAML, JSON, INI, or more as described below)

config.yaml:
```yaml
main:
  listeners:
    - scheme: http
      port: 80
    - scheme: https
      port: 443

metrics:
  listeners:
    - scheme: http
      port: 80

db:
  type: postgres
  uri: jdbc:postgresql://host:port/database
  # The password will be injected from another file
  #password:

root: extra
```

3. Read your config file to an instance of the config model

```scala
@main
def run() =

  // autoset.read[A]() is where the magic happens: it will report any warnings
  // and/or errors, and on success, return the parsed config as well as a config
  // object merged from all config sources
  val (appConfig: AppConfig, raw: autoset.Obj) = autoset.read[AppConfig](
    paths = Seq(os.pwd / "config.yaml"), // explicit config files, parsed by extension
    valueDirs = Seq(os.pwd / "secrets"), // config files whose name corresponds to a key and whose value is taken literally
    envPrefix = "MYAPP_" // allows reading config from environment variables starting with this
  ).getOrElse(sys.exit(1))

  // show a rendered config tree, showing what the application saw and where the
  // config values came from (omitting sensitive fields)
  println(raw.pretty())

  // do something with the parsed config
  println(appConfig)
```

You can see the configuration the application used


```
$ ./app
{ // config.yaml
  main: {
    listeners: [
      {
        scheme: "http",
        port: "80"
      },
      {
        scheme: "https",
        port: "443"
      }
    ],
    motd: null // default
  },
  metrics: {
    listeners: [
      {
        scheme: "http",
        port: "80"
      }
    ],
    motd: null // default
  },
  db: {
    type: "postgres",
    uri: "jdbc:postgresql://host:port/database",
    password: <secret>, // secrets/db.password
    user: "postgres" // default
  },
  root: "extra"
}
...
```

Override by setting an environment variable


```
$ MYAPP_DB_USER=foo ./app
...
db: {
    ...
    user: "foo" // env MYAPP_DB_USER
  },
...
```
