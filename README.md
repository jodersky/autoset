# Autoset

Automatic settings for your application. Read configuration from various
formats and places, and map it to scala types.

Autoset aims to make configuring your application get out of the way, so that
you can focus on writing the important parts.

The idea is simple: define the configuration you need as a case class (or many),
then have autoset read it from files, the environment, or the command line args.
If it succeeds, you get a complete and well-typed configuration which won't fail
when accessed at runtime. If it fails, you'll get a helpful report of the
problem. You'll also be able to inspect what configuration your application is
actually using, along with information on where each config value came from.

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


## Features

- [Parsing config](#parsing-config)
  - [Various formats](#various-formats)
    - [Defining your own format](#defining-your-own-format)
  - [Merge configuration from various places](#merge-configuration-from-various-places)
    - [Merge from multiple files and directories](#merge-from-multiple-files-and-directories)
    - [Value directories](#value-directories)
    - [Environment variables, system props, and args](#environment-variables-system-props-and-args)
  - [Show where your app got your config from](#show-where-your-app-got-your-config-from)
- [Mapping to scala case class](#mapping-to-scala-case-class)
  - [Readers](#readers)
  - [Deriving readers for your own types](#deriving-readers-for-your-own-types)
    - [Case classes](#case-classes)
    - [Sealed types and enums](#sealed-types-and-enums)
    - [Unions of string literals](#unions-of-string-literals)
  - [Writing your own reader](#writing-your-own-reader)
  - [Errors and warnings](#errors-and-warnings)
    - [Collecting diagnostics](#collecting-diagnostics)
  - [Config traits](#config-traits)
    - [Readers for types you don't own](#readers-for-types-you-dont-own)
    - [Settings](#settings)

Autoset reads configuration in two steps. First, every source is parsed into a
single intermediate configuration object (an `autoset.Obj`): a tree of objects,
lists and strings in which every value remembers where it came from. Second,
that object is translated into your own Scala types by readers, which is where
strings become `Int`s, `Duration`s and case classes, where defaults are filled
in, and where anything wrong is reported against the origin recorded in the
first step. The two main sections "Parsing config" and "Mapping to scala case
classes" below follow those two steps.

> [!NOTE]
> All examples in this readme are in the `examples/` folder. You can run each
> with `./mill examples.<name of example>`, for example `./mill examples.demo`.

### Parsing config

#### Various formats

The following formats are provided out-of-the-box:

- YAML (via https://github.com/jodersky/yamlesque)
- INI (built-in parser)
- JSON (via https://github.com/com-lihaoyi/upickle)
- HOCON (aka the typesafe/lightbend config library)
- JVM system properties
- env vars
- arguments

A user can also define their own formats, by implementing a parser.


Every config file is parsed according to its extension.

This example spreads one configuration over one file per supported format,
to show what each of them looks like.

`app.json`
```json
{
  "app": {
    "name": "example",
    "version": "1.0.0"
  }
}
```

`server.yaml`
```yaml
server:
  host: localhost
  ports: [80, 443]
```

`db.ini`
```ini
[db]
url = jdbc:postgresql://localhost/example
pool = 8
```

`logging.conf` (HOCON)
```hocon
logging {
  level = debug
  file = /var/log/example.log
}
```

`limits.properties`
```properties
limits.timeout = 30 seconds
limits.retries = 3
```

Formats which have no notion of types, such as INI, properties files and
environment variables, are read as plain strings. Readers parse those
strings, so `pool = 8` reads as an `Int` just like the JSON number `8`
would.


```scala
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
```

Regardless of the format they were written in, all values end up in the same
configuration:


```
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
```


#### Merge configuration from various places

Configuration is rarely in one place: a package ships defaults, an operator
drops in overrides, an orchestrator mounts secrets, and a deployment sets a
couple of environment variables. Every source is merged into one configuration
object, in a fixed order: files and directories first, then value directories,
then environment variables, system properties and finally command line
arguments.

Merging is recursive for objects, so a source which sets one key of an object
leaves the rest of it alone. Any other value, including a list, replaces what
it overrides entirely.

##### Merge from multiple files and directories


A path given to `autoset.read` may be a file or a directory. Directories are
listed one level deep, in lexicographical order, and every file in them is
parsed by extension. This is the `conf.d` convention found in `/etc`: a base
file, plus a directory of drop-ins which a package or an operator can add to
without editing the base file.

Entries starting with a dot are skipped, so editor swap files and the like
do not take part. Subdirectories are skipped with a warning.

`etc/app.conf`
```hocon
name = example

db {
  host = localhost
  port = 5432
}

logging {
  level = info
}
```

`etc/conf.d/10-db.yaml`
```yaml
db:
  host: db.internal
```

`etc/conf.d/20-logging.ini`
```ini
[logging]
level = debug
```

Files are merged in the order they are read, so later files win. Merging is
recursive for objects only: `10-db.yaml` sets `db.host` without disturbing
`db.port`, which stays at what `app.conf` set. Any other value, including a
list, replaces the one it overrides entirely.


```scala
case class Config(
  name: String,
  db: Db,
  logging: Logging
) derives autoset.Reader

case class Db(host: String, port: Int) derives autoset.Reader
case class Logging(level: String) derives autoset.Reader

@main
def run() =
  val (config, raw) = autoset.read[Config](
    paths = Seq(
      os.pwd / "etc" / "app.conf", // a file, parsed by extension
      os.pwd / "etc" / "conf.d" // a directory of drop-ins, read in order
    )
  ).getOrElse(sys.exit(1))

  println(raw.pretty())
```

The rendered configuration shows which file each value ended up coming from,
and what it overrode:


```
$ ./app
{ // etc/app.conf
  name: "example",
  db: { // etc/conf.d/10-db.yaml
    host: "db.internal", // etc/conf.d/10-db.yaml:2:9 (overrides etc/app.conf:4)
    port: "5432" // etc/app.conf:5
  },
  logging: { // etc/conf.d/20-logging.ini
    level: "debug" // etc/conf.d/20-logging.ini:2:9 (overrides etc/app.conf:9)
  }
}
```


##### Value directories


Secrets are rarely written into config files. They are much more commonly
handed to a process as a directory of files, one file per secret, by
Kubernetes secret volumes, systemd credentials, docker secrets or a
deployment script.

`valueDirs` reads such a directory: a file's *name* is the config path,
split on dots, and its *contents* are the value. A file `db.password` sets
`db.password`.

`config.yaml`
```yaml
db:
  host: localhost
  user: example

api:
  url: https://api.example.com
```

`secrets/db.password`
```
p@ss: #word
```

`secrets/api.token`
```
s3cr3t-token
```

The contents are taken literally rather than parsed, so a password may
contain whatever characters it likes: `p@ss: #word` above would have been a
mapping and a comment had it been written in a YAML file. One trailing
newline is removed, since files usually end with one (`echo` writes one, for
instance).

Entries starting with a dot are skipped, as are subdirectories. This matters
for Kubernetes, which mounts secrets through a hidden `..data` directory.


```scala
case class Config(db: Db, api: Api) derives autoset.Reader

case class Db(
  host: String,
  user: String,
  @autoset.secret password: String
) derives autoset.Reader

case class Api(
  url: java.net.URI,
  @autoset.secret token: String
) derives autoset.Reader

@main
def run() =
  val (config, raw) = autoset.read[Config](
    paths = Seq(os.pwd / "config.yaml"),
    valueDirs = Seq(os.pwd / "secrets")
  ).getOrElse(sys.exit(1))

  println(raw.pretty())

  // the value itself is of course available, verbatim
  println(s"the password is '${config.db.password}'")
```

Values from a value directory take precedence over config files, and are in
turn overridden by environment variables, system properties and arguments.

Fields annotated with `@autoset.secret` are never shown, neither by
`pretty()` nor in error messages, but their origin still is, so it stays
clear which file a secret was picked up from:


```
$ ./app
{ // config.yaml
  db: {
    host: "localhost",
    user: "example",
    password: <secret> // secrets/db.password
  },
  api: {
    url: "https://api.example.com",
    token: <secret> // secrets/api.token
  }
}
the password is 'p@ss: #word'
```


##### Environment variables, system props, and args


Config files describe the common case; a deployment usually needs to change
a setting or two without editing them. Environment variables, system
properties and command line arguments do that, and are applied in that
order, after files and value directories. Arguments therefore win over
everything else.

`config.yaml`
```yaml
db:
  host: localhost
  port: 5432
  user: example

server:
  port: 8080
  workers: 4
```


```scala
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
```

With nothing set, the configuration is what the file says:


```
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
```

Environment variables override files. Both the prefix and the explicit bind
work the same way, and the origin names the variable a value came from:


```
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
```

Since names are split on underscores, a variable cannot reach a key that
contains one. Use `envBinds` for those, or pass an `envKeyReplacer` to spell
the convention out, e.g. splitting on a double underscore instead.

System properties come next. This example app is a launcher script which
passes `JAVA_OPTS` on to the JVM:


```
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
```

Arguments are applied last, so they override everything, including an
environment variable setting the same key:


```
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
```


#### Show where your app got your config from


"Where did that setting come from?" is one of the more annoying questions to
answer about a running service, once config is spread over a file, a
drop-in directory, a secret volume and a handful of environment variables.

Every value keeps the origin it was read from, and the origins of the values
it replaced. `autoset.read` returns the merged configuration alongside the
parsed one, and rendering it shows what the application actually saw, and
where each part of it came from.

`config.yaml`
```yaml
db:
  host: localhost
  port: 5432

logging:
  level: info
  target: /var/log/example.log
```

`secrets/db.password`
```
hunter2
```


```scala
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
```

Note the annotations: a container is labelled with the source most of its
contents come from, and anything that disagrees with that label carries its
own. Values which no source set are filled in with the default of their
field, and marked as such, so that the rendering is the whole configuration
rather than only the parts someone wrote down.

Values of fields annotated with `@autoset.secret` are shown as `<secret>`,
here the password picked up from the secret volume. Their origin is still
shown: it is the value that must not leak, not the fact that it was set.


```
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
```

`pretty(verbose = true)` annotates every value with all of its origins
instead, which is what you want when a value is not what you expect and the
summary is hiding the source you care about:


```
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
```

Since a configuration object is only ever rendered this way — its `toString`
calls `pretty()` — there is no way to log one and leak a secret by accident.


### Mapping to scala case class

#### Readers


A `autoset.Reader[A]` turns a config value into an `A`. Readers for
primitives, for the standard library types that show up in configuration,
and for collections are all available out of the box.

`config.yaml`
```yaml
# primitives
name: example
port: 8080
ratio: 0.75
debug: true

# common standard library types
id: 123e4567-e89b-12d3-a456-426614174000
url: https://example.com/api
addr: "localhost:9090"
timeout: 30 seconds
retention: P30D
startsAt: "04:30"
zone: Europe/Zurich
logFile: log/example.log
pattern: "^[a-z]+$"
workers: 1 to 8
apiKey: c3VwZXItc2VjcmV0
license: TUlU

# collections
hosts: [a.example.com, b.example.com]
weights:
  a: 0.5
  b: 1.5
tags: [red, green, red]
```


```scala
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
```

Relative paths are resolved against the directory of the file they were read
from, so that a config file can refer to its neighbours no matter what the
working directory of the process is. A path from anywhere else, such as an
environment variable, is relative to the working directory instead. Override
`pathRoot` to change that.


```
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
```

Formats which have no types of their own — INI and properties files,
environment variables, system properties and arguments — supply plain
strings. A collection reads such a string by splitting it on commas, so that
a list can be set from a single environment variable:


```
$ MYAPP_HOSTS="c.example.com, d.example.com" ./app
...
hosts     = List(c.example.com, d.example.com)
...
```


#### Deriving readers for your own types


Readers for your own types are derived, either in a `derives` clause or
explicitly with `autoset.readerFor[A]`. Three shapes are supported: case
classes, sealed types (including enums), and unions of string literals.

`config.yaml`
```yaml
server:
  host: 0.0.0.0
  db_url: jdbc:postgresql://localhost/example

storage:
  type: disk
  path: /var/lib/example

cache: memory

level: info
mode: strict
```

##### Case classes

A case class is read from an object, field by field. A field with a default
may be left out; a field without one is required, and missing it is an
error. Keys which match no field are warned about.

Note that an `Option` field is not optional by virtue of being an `Option`:
it may be set to `null`, which reads as `None`, but it must still be set
unless it has a default.


```scala
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
```

##### Sealed types and enums

A sealed trait or enum is read from an object with a `type` key naming the
case, and the rest of the object is read as that case. Nested sealed types
are flattened to their cases. A case without parameters may be written as
just its name, so `cache: memory` is short for `cache: {type: memory}`.

Case names are lowerCamelCase by default, so `InMemory` is written
`inMemory`; both that and the `type` key itself are configurable.


```scala
sealed trait Storage derives autoset.Reader
case class Disk(path: os.Path, sync: Boolean = false) extends Storage
case class S3(bucket: String, @autoset.secret key: String) extends Storage

enum Cache derives autoset.Reader:
  case Memory // `cache: memory`
  case Redis(addr: java.net.InetSocketAddress) // `cache: {type: redis, addr: ...}`

enum Level derives autoset.Reader:
  case Debug, Info, Warn
```

##### Unions of string literals

A union of string literal types reads as one of those strings. This needs no
declaration of its own: unlike the other two shapes, a field written as a
union gets its reader derived on the spot.


```scala
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
```


```
$ ./app
Config(Server(0.0.0.0,8080,jdbc:postgresql://localhost/example,None,None),Disk(/var/lib/example,false),Memory,Info,strict)
```

An object which is only partly set inherits the rest from what it would
otherwise have defaulted to, so setting one key of `server` does not make
the others missing:


```
$ MYAPP_SERVER_PORT=9090 ./app
Config(Server(0.0.0.0,9090,jdbc:postgresql://localhost/example,None,None),Disk(/var/lib/example,false),Memory,Info,strict)
```

Selecting another case of a sealed type replaces it wholesale, since the
fields of the two cases have nothing to do with each other. The keys of the
case which was not selected are then simply unknown:


```
$ MYAPP_STORAGE_TYPE=s3 MYAPP_STORAGE_BUCKET=example MYAPP_STORAGE_KEY=s3cr3t ./app
warning: config.yaml:7:9: unknown key 'storage.path'
Config(Server(0.0.0.0,8080,jdbc:postgresql://localhost/example,None,None),S3(example,s3cr3t),Memory,Info,strict)
```

A deprecated key still works, and says what to write instead:


```
$ MYAPP_SERVER_MOTD=hello ./app
warning: env MYAPP_SERVER_MOTD: key 'server.motd' is deprecated, use 'server.banner' instead
Config(Server(0.0.0.0,8080,jdbc:postgresql://localhost/example,Some(hello),None),Disk(/var/lib/example,false),Memory,Info,strict)
```


#### Writing your own reader


Sooner or later a config needs a type that autoset has never heard of. A
reader is a small trait with one required method, so writing one by hand is
not much work.

`config.yaml`
```yaml
cache:
  size: 512MB

theme:
  background: "0x1e1e2e"
```

Take a byte size, written the way people actually write byte sizes:


```scala
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
```

A reader turns a config value into an `A`, or reports at least one error and
returns `None`. Give it a `given` for the type it reads, and every field of
that type, anywhere in the config, is read with it.


```scala
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
```

`base` is what the value falls back to for anything it leaves out. Readers
of scalars ignore it, since a value which is present replaces what it
overrides entirely; readers of objects fall back key by key, which is how a
partly-set object inherits the rest of its fields.

To read one field differently from the rest of its type, name a reader with
`@autoset.readWith` instead of putting it in scope. The reader has to be a
stable reference, such as a member of an object:


```scala
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
```


```scala
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
```

`entry` was set by no file, so it is shown as what `show` rendered:


```
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
```

And an unparseable value is reported where it was written, in the words the
reader chose:


```
$ MYAPP_CACHE_SIZE=lots ./app
error: env MYAPP_CACHE_SIZE: expected a size (e.g. '512', '4kB' or '1 GiB') for 'cache.size', found 'lots'
```


#### Errors and warnings


Config is written by people, and people make mistakes. autoset reports them
the way a compiler does: with the message, and the place in the file it
applies to.

Everything is read before anything is reported, so one run finds every
problem rather than one per run. Errors mean the config could not be read,
and `read` returns `None`. Warnings mean something was suspicious but
readable — a key nothing expected, or a deprecated one — and the config is
still returned.

`config.yaml`, with three mistakes in it
```yaml
server:
  host: localhost
  prot: 8080

db:
  url: jdbc:postgresql://localhost/example
  pool: many
```


```scala
case class Config(server: Server, db: Db) derives autoset.Reader
case class Server(host: String, port: Int) derives autoset.Reader
case class Db(url: java.net.URI, pool: Int) derives autoset.Reader

def main(args: Array[String]): Unit =
  val file = args.find(!_.startsWith("--")).getOrElse("config.yaml")

  if !args.contains("--collect") then
    // by default, diagnostics are printed to stderr as they are reported
    val (config, _) = autoset.read[Config](
      paths = Seq(os.pwd / file)
    ).getOrElse(sys.exit(1))

    println(config)
```

Reading the file above reports all three problems at once. Note where each
one points: the unknown key at the key itself, the mismatch at the value
that could not be read, and the missing field at the object it should have
been added to, which is the place you would go to fix it.


```
$ ./app
error: config.yaml:2:3: missing required field 'server.port'
warning: config.yaml:3:9: unknown key 'server.prot'
error: config.yaml:7:9: expected an integer for 'db.pool', found 'many'
```

A file which does not parse at all is reported by the parser instead, which
can show the offending line:


```
$ ./app broken.json
error: broken.json:2:43: expected json value got "}"
  "server": {"host": "localhost", "port": },
                                          ^
```

##### Collecting diagnostics

Printing as you go is the default. Pass a plain `autoset.Reporter()` to
collect diagnostics instead, and do what you like with them once loading is
done — count them, render them somewhere other than stderr, or turn them into
log records.


```scala
def collect(file: String) =
  val reporter = autoset.Reporter()

  val result = autoset.read[Config](
    paths = Seq(os.pwd / file),
    reporter = reporter
  )

  println(s"${reporter.errors} error(s), ${reporter.warnings} warning(s)")
  for d <- reporter.diagnostics do
    println(s"  ${d.severity}: ${d.origin.fold("")(_.pretty + ": ")}${d.message}")

  result match
    case Some((config, _)) => println(config)
    case None => sys.exit(1)
```


```
$ ./app --collect
2 error(s), 1 warning(s)
  Error: config.yaml:2:3: missing required field 'server.port'
  Warning: config.yaml:3:9: unknown key 'server.prot'
  Error: config.yaml:7:9: expected an integer for 'db.pool', found 'many'
```


#### Config traits

Readers are not global givens: they are members of an object, and the macro
which derives a reader looks them up on the object it was called on. The same
object carries the settings which decide how Scala names are spelled in config
files. Deriving and reading through your own such object — a *config trait* —
therefore gives you one place to define both.

##### Readers for types you don't own


Everything used so far — `autoset.read`, `autoset.Reader`, the given readers
for primitives and collections — is a member of a single object,
`autoset.default`, which `autoset.<name>` forwards to. `Reader` is a type
*inside* that object, so a `given` written inside another such object is
scoped to it: it does not leak into the rest of the program, and it does not
have to go into the companion of the type it reads.

So: make your own object, put the readers your application needs in it, and
read through it.

`config.yaml`
```yaml
name: example
locale: de-CH
charset: UTF-8
```


```scala
object cfg extends autoset.Api with autoset.DefaultReaders:

  // `java.util.Locale` is not ours, so we cannot put a reader in its
  // companion object. Here it is simply a member of `cfg`
  given Reader[java.util.Locale] with
    def read(value: autoset.Value, base: Option[java.util.Locale], ctx: autoset.model.Context) =
      value match
        case autoset.Str(raw, _, _) =>
          java.util.Locale.forLanguageTag(raw.trim) match
            case l if l.getLanguage.isEmpty =>
              autoset.ReaderUtils.mismatch("a language tag (e.g. 'de-CH')", value, ctx)
            case l => Some(l)
        case _ => autoset.ReaderUtils.mismatch("a language tag (e.g. 'de-CH')", value, ctx)

    override def show(a: java.util.Locale) =
      Some(autoset.Str(a.toLanguageTag, autoset.LitKind.String, Nil))

  given Reader[java.nio.charset.Charset] with
    def read(value: autoset.Value, base: Option[java.nio.charset.Charset], ctx: autoset.model.Context) =
      value match
        case autoset.Str(raw, _, _) if java.nio.charset.Charset.isSupported(raw.trim) =>
          Some(java.nio.charset.Charset.forName(raw.trim))
        case _ => autoset.ReaderUtils.mismatch("a charset (e.g. 'UTF-8')", value, ctx)

    override def show(a: java.nio.charset.Charset) =
      Some(autoset.Str(a.name, autoset.LitKind.String, Nil))
```

Derive with `cfg.Reader` rather than `autoset.Reader`, so that the macro
looks readers up in `cfg`. Deriving with `autoset.Reader` here would fail:
`autoset.default` has never heard of a `Locale`.


```scala
case class Config(
  name: String, // read with cfg.StringReader, inherited from DefaultReaders
  locale: java.util.Locale, // read with the given above
  charset: java.nio.charset.Charset
) derives cfg.Reader

@main
def run() =
  // and read through `cfg` too, not through `autoset`
  val (config, raw) = cfg.read[Config](
    paths = Seq(os.pwd / "config.yaml")
  ).getOrElse(sys.exit(1))

  println(config)
```


```
$ ./app
Config(example,de_CH,UTF-8)
```

One object holding every reader an application needs is also just a good
place to look: there is one answer to "how is this type read", and it is not
scattered over companion objects. The next section uses the same object for
the other half of the pattern, the settings.


##### Settings


The other half of the configuration traits pattern: the conventions of how
Scala names are spelled in config files are settings on the same object, so
overriding one changes every reader derived through it, rather than every
call site.

`app.cfg`
```yaml
db:
  max_pool_size: 16
  connect_timeout: 5 seconds

storage:
  kind: object-store
  bucket: example
```


```scala
object cfg extends autoset.Api with autoset.DefaultReaders:

  // config files are snake_case, Scala is camelCase: `maxPoolSize` is read
  // from the key `max_pool_size`. `ReaderUtils.kebabify` is there too, and
  // anything else is just a `String => String`
  override def fieldName(name: String) = autoset.ReaderUtils.snakify(name)

  // the same for the cases of sealed types, which are lowerCamelCase by
  // default: `ObjectStore` is written `object-store`
  override def caseName(name: String) = autoset.ReaderUtils.kebabify(name)

  // the key which says which case of a sealed type an object is. `type` by
  // default
  override def discriminator = "kind"

  // how the name of an environment variable becomes a config path. Splitting
  // on a double underscore leaves single ones to the keys, so
  // `MYAPP_DB__MAX_POOL_SIZE` sets `db.max_pool_size`
  override def defaultEnvKeyReplacer = _.toLowerCase.split("__").toList

  // our config files are called `*.cfg`, and they are YAML
  override def defaultParsers = super.defaultParsers + ("cfg" -> autoset.parsers.YamlParser)
```

`fieldName`, `caseName` and `discriminator` are baked into every reader
derived through `cfg`. The two `default*` members are what `read` falls back
to when its `parsers` and `envKeyReplacer` parameters are not given, so a
single call can still deviate from them.


```scala
case class Config(db: Db, storage: Storage) derives cfg.Reader

case class Db(
  maxPoolSize: Int, // `max_pool_size`
  connectTimeout: scala.concurrent.duration.Duration // `connect_timeout`
) derives cfg.Reader

sealed trait Storage derives cfg.Reader
case class LocalDisk(path: os.Path) extends Storage // `{kind: local-disk, ...}`
case class ObjectStore(bucket: String) extends Storage // `{kind: object-store, ...}`

@main
def run() =
  val (config, _) = cfg.read[Config](
    paths = Seq(os.pwd / "app.cfg"),
    envPrefix = "MYAPP_"
  ).getOrElse(sys.exit(1))

  println(config)
```


```
$ ./app
Config(Db(16,5 seconds),ObjectStore(example))
```

The same conventions apply to environment variables, since the settings live
on the object rather than on the call:


```
$ MYAPP_DB__MAX_POOL_SIZE=32 MYAPP_STORAGE__KIND=local-disk MYAPP_STORAGE__PATH=/data ./app
warning: app.cfg:7:11: unknown key 'storage.bucket'
Config(Db(32,5 seconds),LocalDisk(/data))
```


## Building

This project is built with [mill](https://mill-build.org).

Some common tasks are also exposed in the `scripts/` directory, for example
`./scripts/test` to run all tests.

The library is available for Scala 3 on the JVM and native. It is published to
maven central:

```scala
mvn"io.crashbox::autoset::0.1.0"
```

## Notice about LLM usage

This project was built with the assistance of a large language model. Many
examples and tests were generated. However, the core logic was hand coded
initially and changes driven by specs.