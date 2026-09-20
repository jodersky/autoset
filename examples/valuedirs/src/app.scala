package example

// Secrets are rarely written into config files. They are much more commonly
// handed to a process as a directory of files, one file per secret, by
// Kubernetes secret volumes, systemd credentials, docker secrets or a
// deployment script.
//
// `valueDirs` reads such a directory: a file's *name* is the config path,
// split on dots, and its *contents* are the value. A file `db.password` sets
// `db.password`.

// `config.yaml`
// ```yaml
//include:../config.yaml
// ```

// `secrets/db.password`
// ```
//include:../secrets/db.password
// ```

// `secrets/api.token`
// ```
//include:../secrets/api.token
// ```

// The contents are taken literally rather than parsed, so a password may
// contain whatever characters it likes: `p@ss: #word` above would have been a
// mapping and a comment had it been written in a YAML file. One trailing
// newline is removed, since files usually end with one (`echo` writes one, for
// instance).
//
// Entries starting with a dot are skipped, as are subdirectories. This matters
// for Kubernetes, which mounts secrets through a hidden `..data` directory.

//snippet:start
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
//snippet:end

// Values from a value directory take precedence over config files, and are in
// turn overridden by environment variables, system properties and arguments.
//
// Fields annotated with `@autoset.secret` are never shown, neither by
// `pretty()` nor in error messages, but their origin still is, so it stays
// clear which file a secret was picked up from:

/* usage snippet
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
*/
