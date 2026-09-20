package example

// The other half of the configuration traits pattern: the conventions of how
// Scala names are spelled in config files are settings on the same object, so
// overriding one changes every reader derived through it, rather than every
// call site.

// `app.cfg`
// ```yaml
//include:../app.cfg
// ```

//snippet:start
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
//snippet:end

// `fieldName`, `caseName` and `discriminator` are baked into every reader
// derived through `cfg`. The two `default*` members are what `read` falls back
// to when its `parsers` and `envKeyReplacer` parameters are not given, so a
// single call can still deviate from them.

//snippet:start
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
//snippet:end

/* usage snippet
$ ./app
Config(Db(16,5 seconds),ObjectStore(example))
*/

// The same conventions apply to environment variables, since the settings live
// on the object rather than on the call:

/* usage snippet
$ MYAPP_DB__MAX_POOL_SIZE=32 MYAPP_STORAGE__KIND=local-disk MYAPP_STORAGE__PATH=/data ./app
warning: app.cfg:7:11: unknown key 'storage.bucket'
Config(Db(32,5 seconds),LocalDisk(/data))
*/
