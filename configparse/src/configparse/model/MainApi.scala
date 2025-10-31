package configparse.model

import configparse.formats

trait MainApi:

  /** Configuration readers for file extensions.
    *
    * This can be overridden to globally change the behavior instead of
    * overriding it in every call of `read`. */
  def defaultReaders: Map[String, formats.FileReader] = Map(
    "json" -> formats.JsonReader,
    "yaml" -> formats.YamlReader,
    "properties" -> formats.PropsReader,
    "conf" -> formats.HoconReader,
    "ini" -> formats.IniReader,
    "" -> formats.IniReader
  )

  /** A function to transform the name of an environment variable into a
    * configuration path.
    *
    * This can be overridden to globally change the behavior instead of
    * overriding it in every call of `read`. */
  def defaultEnvKeyReplacer: String => String = s => s.replaceAll("_", ".").toLowerCase

  /** Read a configuration from various sources and formats.
    *
    * The configuration will be the result of merging configuration objects in
    * the order of the parameters of this method. I.e.
    *
    * - start by reading files and directories
    * - add configurations from environment variables
    * - add configurations from system properties
    * - add configuration from command line arguments
    *
    * Merging of configuration objects means that object's keys are merged
    * recursively. Other types however are replaced. Example:
    *
    * Original
    *
    * ```
    * {
    *   "a": "lhs"
    *   "b": {
    *      "inner": {
    *         "foo": "lhs"
    *       }
    *    }
    * }
    * ```
    *
    * merge with
    *
    * ```
    * {
    *   "b": {
    *      "inner": {
    *         "foo": "rhs"
    *       }
    *    }
    *   "c": "rhs"
    * }
    * ```
    *
    * results in
    *
    * ```
    * {
    *   "a": "lhs"
    *   "b": {
    *      "inner": {
    *         "foo": "rhs"
    *       }
    *    }
    *   "c": "rhs"
    * }
    * ```
    *
    * @param paths Paths of configuration files or directories containing config
    *   files. In case a path is a directory, it will be traversed for config
    *   files sorted in lexicographical order (only one level deep). In case a
    *   path is a file, it will be parsed based on extension. See the `readers`
    *   param.
    *
    * @param pwd Root directory to use if files are given as relative paths.
    * Defaults to current working directory.
    *
    * @param readers Additional readers for file extensions. See
    * `defaultReaders` in the API trait.
    *
    * @param env Environment variables available for reading. Note that by
    * default none will be read, unless other `env*` parameters are specified.
    * Defaults to system environment variables.
    *
    * @param envPrefix Automatically read environment variables starting with
    * this prefix, if set. Any environment variables read in such a way will
    * have the prefix stripped before being transformed into a configuration
    * path via the `envKeyReplacer`.
    *
    * For example if the env contains `APP_FOO_BAR=1` and the prefix is given as
    * `APP_`, then this will result in the configuration `foo.bar=1`.
    *
    * @param envKeyReplacer A function to transform the name of an environment
    * variable into a configuration path. This overrides the default
    * `defaultEnvKeyReplacer` if set.
    *
    * @param envBinds An association of environment variables to configuration
    * paths. This is used to manually bind environment variables, for example if
    * the prefix approach cannot be used.
    *
    * For example if the env contains `SOME_SETTING=1` and the bindings contain
    * `"SOME_SETTING" -> "foo.bar"`, then this will result in the configuration
    * `foo.bar=1`
    *
    * @param props System properties available for reading. Note that by default
    * none will be read, unless other `props*` parameters are specified.
    * Defaults to the current runtime's properties (on the JVM).
    *
    * @param propsPrefix Automatically read system properties starting with this
    * prefix, if set. Any properties read in such a way will have the prefix
    * stripped before being read as a configuration path.
    *
    * For example if the properties contain `app.foo.bar=1` and the prefix is
    * given as `app.`, then this will result in the configuration `foo.bar=1`.
    *
    * @param propsBinds An association of properties to configuration paths.
    * This is used to manually bind properties, for example if the prefix
    * approach cannot be used.
    *
    * For example if the properties contain `some.property=1` and the bindings
    * contain `"some.property" -> "foo.bar"`, then this will result in the
    * configuration `foo.bar=1`
    *
    * @param args Command line arguments that should be interpreted as
    * configurations.
    *
    * @param dest Root configuration object into which all other configuration
    * will be merged. This can be set to build configurations through multiple
    * calls to `read`. Defaults to an empty configuration.
    */
  def readConfig(
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
    dest: Config = Config()
  ): Config =
    def readfile(file: os.FilePath, reader: formats.FileReader) =
      val abs = os.Path(file, pwd)

      if !os.exists(abs) then throw ReadException(s"config file $file does not exist.")
      if !os.isFile(abs) then throw ReadException(s"config file $file is not a file.")
      var stream: java.io.InputStream = null
      val result =
        try
          stream = os.read.inputStream(abs)
          reader.read(file.toString, stream, 0)
        catch
          case ex => throw ReadException(s"Error reading config file $file", ex)
        finally
          if stream != null then stream.close()
      result match
        case formats.FileReader.Result.Error(message, row, col, line) =>
          val pos =
            if row > 0 && col > 0 then
              s"$file:$row:$col"
            else if row > 0 then
              s"$file:$row"
            else file.toString

          val line1 = if line == "" then "" else ("\n" + line)
          val caret = if col > 0 && line != "" then "\n" + " " * (col - 1) + "^" else ""

          throw ReadException(s"error parsing config file $pos: $message$line1$caret")
        case formats.FileReader.Result.Success(obj) =>
          dest.mergeFrom(obj)

    val allReaders = defaultReaders //++ readers

    // files and directories
    for root <- paths do
      val absRoot = os.Path(root, pwd)
      if os.isDir(absRoot) then
        for
          path <- os.list(absRoot, sort = true)
          if os.isFile(path)
          if path.baseName != "" // ignore files starting with "." (aka hidden files)
          if allReaders.contains(path.ext)
        do
          root match
            case abs: os.Path =>
              readfile(path, allReaders(path.ext))
            case _ =>
              readfile(path.relativeTo(pwd), allReaders(path.ext))

      else
        val reader = allReaders.getOrElse(
          root.ext.toLowerCase(),
          throw ReadException(s"config file ${root} has an unknown format. Allowed file extensions are ${allReaders.keySet.mkString(", ")}")
        )
        readfile(root, reader)

    // automatic env
    if envPrefix != null then
      for (envKey, envValue) <- env if envKey.startsWith(envPrefix) do
        val configKey = envKeyReplacer(envKey.drop(envPrefix.length))
        dest.set(configKey, envValue, Origin.Env(envKey))

    // explicit env
    for (envKey, configKey) <- envBinds do
      for envValue <- env.get(envKey) do
        dest.set(configKey, envValue, Origin.Env(envKey))

    // automatic props
    if propsPrefix != null then
      for (propsKey, propsValue) <- props if propsKey.startsWith(propsPrefix) do
        val configKey = propsKey.drop(propsPrefix.length)
        dest.set(configKey, propsValue, Origin.Props(propsKey))

    // explicit props
    for (propsKey, configKey) <- propsBinds do
      for propsValue <- props.get(propsKey) do
        dest.set(configKey, propsValue, Origin.Props(propsKey))

    // args
    for (argKey, argValue) <- args do
      dest.set(argKey, argValue, Origin.Arg())

    dest

  end readConfig
