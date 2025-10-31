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
    reader.read(cfg, Nil) match
      case Result.Success(a)   => a
      case Result.Error(errs*) =>
        throw ReadException(errs.map(_.pretty).mkString("\n"))
