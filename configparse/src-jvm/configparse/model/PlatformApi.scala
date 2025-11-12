package configparse.model

import configparse.formats.FileReader

trait PlatformApi:
  self: MainApi =>

  def watch(
      paths: Iterable[os.FilePath] = Seq(),
      pwd: os.Path = os.pwd,
      readers: Map[String, FileReader] = Map(),
      env: Map[String, String] = sys.env,
      envPrefix: String = null,
      envKeyReplacer: String => String = defaultEnvKeyReplacer,
      envBinds: Iterable[(String, String)] = Map(),
      props: collection.Map[String, String] = sys.props,
      propsPrefix: String = null,
      propsBinds: Iterable[(String, String)] = Map(),
      args: Iterable[(String, String)] = Map(),
      config: Config = Config(),
      onPreUpdate: Set[os.Path] => Unit = _ => (),
      onUpdate: (Config, Config) => Unit,
      onError: ReadException => Unit
  ): java.io.Closeable =

    @volatile var oldCfg = config
    def fn() =
      try
        val newCfg = readConfig(
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
        onUpdate(oldCfg, newCfg)
        oldCfg = newCfg
      catch case ex: ReadException => onError(ex)

    fn()
    Watcher(
      paths.map(fp => os.Path(fp, pwd)),
      changed =>
        onPreUpdate(changed)
        fn()
    )
