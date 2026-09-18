package autoset.main

import autoset.parsers
import autoset.model
import autoset.model.Obj
import autoset.model.Origin
import autoset.model.LitKind
import autoset.model.Str


trait Api extends autoset.derivation.ReadersApi:

  /** Configuration parsers for file extensions.
    *
    * This can be overridden to globally change the behavior instead of
    * overriding it in every call of `read`.
    */
  def defaultParsers: Map[String, autoset.model.FormatParser] = Map(
    "json" -> parsers.JsonParser,
    "yaml" -> parsers.YamlParser,
    "yml" -> parsers.YamlParser,
    "properties" -> parsers.PropsParser,
    "conf" -> parsers.HoconParser,
    "ini" -> parsers.IniParser,
    "" -> parsers.IniParser
  )

  /** A function to transform the name of an environment variable into a
    * configuration path.
    *
    * This can be overridden to globally change the behavior instead of
    * overriding it in every call of `read`.
    */
  def defaultEnvKeyReplacer: String => List[String] = s =>
    s.toLowerCase().split("_").toList

  /** Read a configuration from various sources and formats.
    *
    * The configuration will be the result of merging configuration objects in
    * the order of the parameters of this method. I.e.
    *
    *   - start by reading files and directories
    *   - add configurations from environment variables
    *   - add configurations from system properties
    *   - add configuration from command line arguments
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
    * @param paths
    *   Paths of configuration files or directories containing config files. In
    *   case a path is a directory, it will be traversed for config files sorted
    *   in lexicographical order (only one level deep). In case a path is a
    *   file, it will be parsed based on extension. See the `readers` param.
    *
    * @param pwd
    *   Root directory to use if files are given as relative paths. Defaults to
    *   current working directory.
    *
    * @param parsers
    *   Parsers for file extensions. Defaults to `defaultParsers`; to add a
    *   format, pass `defaultParsers + ("ext" -> parser)`.
    *
    * @param env
    *   Environment variables available for reading. Note that by default none
    *   will be read, unless other `env*` parameters are specified. Defaults to
    *   system environment variables.
    *
    * @param envPrefix
    *   Automatically read environment variables starting with this prefix, if
    *   set. Any environment variables read in such a way will have the prefix
    *   stripped before being transformed into a configuration path via the
    *   `envKeyReplacer`.
    *
    *   For example if the env contains `APP_FOO_BAR=1` and the prefix is given
    *   as `APP_`, then this will result in the configuration `foo.bar=1`.
    *
    * @param envKeyReplacer
    *   A function to transform the name of an environment variable into a
    *   configuration path. This overrides the default `defaultEnvKeyReplacer`
    *   if set.
    *
    * @param envBinds
    *   An association of environment variables to configuration paths. This is
    *   used to manually bind environment variables, for example if the prefix
    *   approach cannot be used.
    *
    *   For example if the env contains `SOME_SETTING=1` and the bindings contain
    *   `"SOME_SETTING" -> List("foo", "bar")`, then this will result in the configuration
    *   `foo.bar=1`
    *
    * @param props
    *   System properties available for reading. Note that by default none will
    *   be read, unless other `props*` parameters are specified. Defaults to the
    *   current runtime's properties (on the JVM).
    *
    * @param propsPrefix
    *   Automatically read system properties starting with this prefix, if set.
    *   Any properties read in such a way will have the prefix stripped before
    *   being read as a configuration path.
    *
    *   For example if the properties contain `app.foo.bar=1` and the prefix is
    *   given as `app.`, then this will result in the configuration `foo.bar=1`.
    *
    * @param propsBinds
    *   An association of properties to configuration paths. This is used to
    *   manually bind properties, for example if the prefix approach cannot be
    *   used.
    *
    *   For example if the properties contain `some.property=1` and the bindings
    *   contain `"some.property" -> List("foo", "bar")`, then this will result in the
    *   configuration `foo.bar=1`
    *
    * @param init
    *   Root configuration object into which all other configuration will be
    *   merged. This can be set to build configurations through multiple calls
    *   to `load`. Defaults to an empty configuration. Note that it is modified
    *   in place, even if loading fails.
    *
    * @param reporter
    *   Where to report warnings and errors.
    *
    * @return
    *   The merged configuration, or `None` if any errors were reported. All
    *   files are parsed even if one fails, so that all errors are reported at
    *   once.
    */
  def load(
    paths: Iterable[os.FilePath] = Seq(),
    pwd: os.Path = os.pwd,
    parsers: Map[String, autoset.model.FormatParser] = defaultParsers,
    env: Map[String, String] = sys.env,
    envPrefix: String = null,
    envKeyReplacer: String => List[String] = defaultEnvKeyReplacer,
    envBinds: Iterable[(String, List[String])] = Map(),
    props: collection.Map[String, String] = sys.props,
    propsPrefix: String = null,
    propsBinds: Iterable[(String, List[String])] = Map(),
    // args: Iterable[(List[String], String)] = Map(),
    init: model.Obj = model.Obj(
      collection.mutable.LinkedHashMap.empty,
      List(model.Origin.Default) // TODO: use special "root" origin?
    ),
    reporter: model.Reporter = model.Reporter()
  ): Option[model.Obj] =

    // the reporter may be shared, so only count errors from this call
    val initialErrors = reporter.errors
    def failed = reporter.errors > initialErrors

    val files = collection.mutable.ListBuffer.empty[os.Path]
    for path <- paths do
      val abs = os.Path(path, pwd)
      if !os.exists(abs) then reporter.error(s"path $path does not exist")
      else if os.isDir(abs) then
        for p <- os.list(abs, sort = true) if !p.last.startsWith(".") do
          if os.isFile(p) then files += p
          else
            reporter.warn(
              s"skipping ${p.relativeTo(pwd)} in config directory $path, which is not a regular file"
            )
      else if os.isFile(abs) then files += abs
      else reporter.error(s"cannot read $path, which is not a file or directory")
    if failed then return None

    for file <- files do
      val name = file.relativeTo(pwd).toString
      parsers.get(file.ext) match
        case None => reporter.error(s"no parser defined for extension of file $name")
        case Some(parser) =>
          val stream = os.read.inputStream(file)
          try
            // keep parsing after an error, to report all of them at once, but
            // don't bother merging
            for obj <- parser.parse(name, stream, os.stat(file).size.toInt, reporter)
            if !failed do merge(init, obj, reporter)
          finally stream.close()
    if failed then return None

    def fromEnv(key: String, path: List[String]) =
      setConfig(init, path, Str(env(key), LitKind.Unknown, List(Origin.Env(key))), reporter)

    def fromProps(key: String, path: List[String]) =
      setConfig(init, path, Str(props(key), LitKind.Unknown, List(Origin.Props(key))), reporter)

    // sorted, so that keys which override each other are applied in a stable
    // order (e.g. APP_DB and APP_DB_HOST)
    if envPrefix != null then
      for key <- env.keys.toSeq.sorted if key.startsWith(envPrefix) do
        fromEnv(key, envKeyReplacer(key.drop(envPrefix.length)))
    for (key, path) <- envBinds if env.contains(key) do fromEnv(key, path)

    if propsPrefix != null then
      for key <- props.keys.toSeq.sorted if key.startsWith(propsPrefix) do
        fromProps(key, key.drop(propsPrefix.length).split("\\.", -1).toList)
    for (key, path) <- propsBinds if props.contains(key) do fromProps(key, path)

    // args
    // for (argKey, argValue) <- args do
    //   setConfig(init, argKey, Str(propsValue, LitKind.Unknown, List(Origin.Props(propsKey))), reporter)

    if failed then None else Some(init)

  /** Merge the fields of `from` into `into`.
    *
    * Note that only objects are merged. Any other values will overwrite
    * existing ones, and the overwritten values' origins are appended to the
    * new value's `origins`.
    *
    * A warning is reported when an object is replaced by a value or vice
    * versa, since that usually means a key was set at the wrong level, e.g.
    * `db=x` when `db` is an object with nested keys.
    *
    * This consumes `from`: its subtrees are moved into `into`, so it must not
    * be used or modified afterwards.
    */
  def merge(into: model.Obj, from: model.Obj, reporter: model.Reporter): Unit =
    mergeAt(Nil, into, from, reporter)

  private def mergeAt(
      path: List[String],
      into: model.Obj,
      from: model.Obj,
      reporter: model.Reporter
  ): Unit =
    for (k, theirs) <- from.fields do
      (into.fields.get(k), theirs) match
        case (None, _) => into.fields(k) = theirs
        case (Some(mine: model.Obj), theirs: model.Obj) =>
          mergeAt(k :: path, mine, theirs, reporter)
        case (Some(mine), _) =>
          warnReplaced((k :: path).reverse, mine, theirs, reporter)
          theirs.origins = theirs.origins ::: mine.origins
          into.fields(k) = theirs
    into.origins = from.origins ::: into.origins

  private def warnReplaced(
      path: List[String],
      mine: model.Value,
      theirs: model.Value,
      reporter: model.Reporter
  ): Unit =
    def describe(v: model.Value) = v match
      case _: model.Obj => "an object"
      case _: model.Arr => "a list"
      case _ => "a value"
    // null is a placeholder, so replacing it (or with it) is expected
    val structural = (mine, theirs) match
      case (_: model.Null, _) | (_, _: model.Null) => false
      case (_: model.Obj, _) | (_, _: model.Obj) => true
      case _ => false
    if structural then
      reporter.warn(
        s"'${path.mkString(".")}' is set to ${describe(theirs)}, replacing " +
          s"${describe(mine)} from ${mine.effectiveOrigin.pretty}",
        theirs.effectiveOrigin
      )

  /** Set `value` at `path`, creating intermediate objects as needed. This has
    * the same semantics as merging an object containing only that value, see
    * [[merge]].
    */
  private def setConfig(
      obj: model.Obj,
      path: List[String],
      value: model.Value,
      reporter: model.Reporter
  ): Unit =
    if path.isEmpty || path.exists(_.isEmpty) then
      reporter.warn(
        s"ignoring invalid configuration key '${path.mkString(".")}'",
        value.effectiveOrigin
      )
    else
      var curr = obj
      curr.origins = value.origins ::: curr.origins
      for (segment, i) <- path.init.zipWithIndex do
        curr = curr.fields.get(segment) match
          case Some(o: model.Obj) =>
            o.origins = value.origins ::: o.origins
            o
          case existing =>
            val o = model.Obj(collection.mutable.LinkedHashMap.empty, value.origins)
            for e <- existing do
              warnReplaced(path.take(i + 1), e, o, reporter)
              o.origins = o.origins ::: e.origins
            curr.fields(segment) = o
            o

      (curr.fields.get(path.last), value) match
        case (None, _) => curr.fields(path.last) = value
        case (Some(o: model.Obj), v: model.Obj) => mergeAt(path.reverse, o, v, reporter)
        case (Some(e), _) =>
          warnReplaced(path, e, value, reporter)
          value.origins = value.origins ::: e.origins
          curr.fields(path.last) = value
