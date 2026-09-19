
case class ServerConfig(
  address: String,
  port: Int,
  motd: Option[String],
  db: DatabaseConfig
) derives autoset.Reader

enum DatabaseConfig derives autoset.Reader:
  case Sqlite(file: os.Path)
  case Postgres(
    uri: java.net.URI,
    @autoset.secret password: String
  )

@main
def run() =
  autoset.read[ServerConfig](
    paths = Seq(os.pwd / "config.yaml")
  ) match
    case None =>
      System.err.println("error in config file")
    case Some((serverConfig, raw)) =>
      println(raw.pretty())
