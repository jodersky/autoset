package configparse.derivation

import configparse.formats
import configparse.model.Config
import configparse.model.Value
import configparse.model.Path
import configparse.model.ReadException

trait Api
    extends configparse.model.MainApi
    with ReaderApi
    with StandardReaders
    with DerivationApi:

  def readResult[A](
      paths: Iterable[os.FilePath] = Seq(),
      pwd: os.Path = os.pwd,
      readers: Map[String, formats.FileReader] = Map(),
      env: Map[String, String] = sys.env,
      envPrefix: String = null,
      envKeyReplacer: String => String = defaultEnvKeyReplacer,
      envBinds: Iterable[(String, String)] = Map(),
      props: collection.Map[String, String] = sys.props,
      propsPrefix: String = null,
      propsBinds: Iterable[(String, String)] = Map(),
      args: Iterable[(String, String)] = Map(),
      config: Config = Config()
  )(using reader: Reader[A]): Result[A] =
    val cfg = readConfig(
      paths,
      pwd,
      readers,
      env,
      envPrefix,
      envKeyReplacer,
      envBinds,
      props,
      propsPrefix,
      propsBinds,
      args,
      config
    )
    reader.read(cfg, Nil)

  def read[A](
      paths: Iterable[os.FilePath] = Seq(),
      pwd: os.Path = os.pwd,
      readers: Map[String, formats.FileReader] = Map(),
      env: Map[String, String] = sys.env,
      envPrefix: String = null,
      envKeyReplacer: String => String = defaultEnvKeyReplacer,
      envBinds: Iterable[(String, String)] = Map(),
      props: collection.Map[String, String] = sys.props,
      propsPrefix: String = null,
      propsBinds: Iterable[(String, String)] = Map(),
      args: Iterable[(String, String)] = Map(),
      config: Config = Config()
  )(using reader: Reader[A]): A =
    readResult[A](
      paths,
      pwd,
      readers,
      env,
      envPrefix,
      envKeyReplacer,
      envBinds,
      props,
      propsPrefix,
      propsBinds,
      args,
      config
    ) match
      case Result.Success(a)              => a
      case Result.Error(missing, invalid) =>
        val b = new StringBuilder
        b ++= "error reading configuration:\n"
        for path <- missing do b ++= s"missing required field '$path'\n"
        for inv <- invalid do
          b ++= s"invalid value in configuration '${inv.path}': ${inv.message}\n"
          inv.value.origins match
            case Nil       => ()
            case head :: _ =>
              b ++= s"  from ${head.pretty}\n"
        throw ReadException(b.toString())
