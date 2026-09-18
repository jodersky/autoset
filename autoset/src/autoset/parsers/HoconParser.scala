package autoset.parsers

import autoset.model.*
import collection.mutable as m
import org.ekrich.config
import scala.jdk.CollectionConverters.*

/** Parses HOCON files.
  *
  * Substitutions are resolved within the file only; environment variables are
  * not consulted, since they are a separate source of configuration. Includes
  * are not supported.
  *
  * Positions only have line numbers, and numbers are normalized (e.g. `1e3`
  * becomes `1000`), since that is what the underlying library provides.
  */
object HoconParser extends FormatParser:

  private def convert(name: String, ls: LocationStream, value: config.ConfigValue): Value =
    val origins = List(ls.lineOrigin(name, value.origin.lineNumber))
    value match
      case obj: config.ConfigObject =>
        // the library doesn't preserve key order, so restore it from the source
        val fields = obj.asScala.toSeq.sortBy((k, v) => (v.origin.lineNumber, k))
        Obj(m.LinkedHashMap.from(fields.map((k, v) => k -> convert(name, ls, v))), origins)
      case list: config.ConfigList =>
        Arr(m.ListBuffer.from(list.asScala.map(convert(name, ls, _))), origins)
      case _ =>
        value.valueType match
          case config.ConfigValueType.NULL => Null(origins)
          case config.ConfigValueType.STRING =>
            Str(value.unwrapped.toString, LitKind.String, origins)
          case config.ConfigValueType.NUMBER =>
            Str(value.unwrapped.toString, LitKind.Num, origins)
          case _ => Str(value.unwrapped.toString, LitKind.Bool, origins)

  def parse(
      name: String,
      stream: java.io.InputStream,
      sizeHint: Int,
      reporter: Reporter
  ): Option[Obj] =
    val ls = LocationStream(stream, sizeHint)
    val options = config.ConfigParseOptions.defaults
      .setOriginDescription(name)
      .setSyntax(config.ConfigSyntax.CONF)
    try
      val reader = java.io.InputStreamReader(ls, java.nio.charset.StandardCharsets.UTF_8)
      val hocon = config.ConfigFactory
        .parseReader(reader, options)
        .resolve(config.ConfigResolveOptions.noSystem)
      Some(convert(name, ls, hocon.root).asInstanceOf[Obj])
    catch
      case ex: config.ConfigException =>
        ls.drain()
        val line = if ex.origin == null then -1 else ex.origin.lineNumber
        // the message is prefixed with the origin, which we report separately
        val prefix = if ex.origin == null then "" else ex.origin.description + ":"
        val message = ex.getMessage.stripPrefix(prefix).trim
        reporter.error(message, ls.lineOrigin(name, line), ls.lineText(line))
        None
