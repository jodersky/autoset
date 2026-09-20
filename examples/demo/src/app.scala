//


//include:config.yaml

//snippet:start
case class AppConfig(
  main: ServerConfig,
  metrics: ServerConfig,
  db: DatabaseConfig,
  root: os.Path
) derives autoset.Reader
//setenv:MY_APP=e

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
//snippet:end

@main
def run() =
  val (appConfig: AppConfig, raw: autoset.Obj) = autoset.read[AppConfig](
    paths = Seq(os.pwd / "config.yaml"),
    valueDirs = Seq(os.pwd / "secrets"),
    envPrefix = "MYAPP_"
  ).getOrElse(sys.exit(1))
  println(raw.origins)

  println(raw.pretty())

/* usage
$ MYAPP_app
*/
