package example

// A path given to `autoset.read` may be a file or a directory. Directories are
// listed one level deep, in lexicographical order, and every file in them is
// parsed by extension. This is the `conf.d` convention found in `/etc`: a base
// file, plus a directory of drop-ins which a package or an operator can add to
// without editing the base file.
//
// Entries starting with a dot are skipped, so editor swap files and the like
// do not take part. Subdirectories are skipped with a warning.

// `etc/app.conf`
// ```hocon
//include:../etc/app.conf
// ```

// `etc/conf.d/10-db.yaml`
// ```yaml
//include:../etc/conf.d/10-db.yaml
// ```

// `etc/conf.d/20-logging.ini`
// ```ini
//include:../etc/conf.d/20-logging.ini
// ```

// Files are merged in the order they are read, so later files win. Merging is
// recursive for objects only: `10-db.yaml` sets `db.host` without disturbing
// `db.port`, which stays at what `app.conf` set. Any other value, including a
// list, replaces the one it overrides entirely.

//snippet:start
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
//snippet:end

// The rendered configuration shows which file each value ended up coming from,
// and what it overrode:

/* usage snippet
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
*/
