package autoset.parsers

import autoset.model.*
import collection.mutable as m

/** Parses Java properties files.
  *
  * Dotted keys are nested into objects: `a.b=1` is the same as the object
  * `{a: {b: 1}}`. Values are plain text, so their kind is [[LitKind.Unknown]].
  * The file is read as UTF-8.
  *
  * The underlying library does not report positions, so origins only name the
  * file.
  */
object PropsParser extends FormatParser:

  def parse(
      name: String,
      stream: java.io.InputStream,
      sizeHint: Int,
      reporter: Reporter
  ): Option[Obj] =
    val origin = Origin.File(name, -1, -1, -1)
    val root = Obj(m.LinkedHashMap(), List(origin))

    def set(key: String, value: String): Unit =
      val segments = key.split("\\.", -1).toList
      if segments.exists(_.isEmpty) then
        reporter.warn(s"ignoring key '$key', which has an empty segment", origin)
        return
      var obj = root
      for seg <- segments.init do
        obj = obj.fields.get(seg) match
          case Some(o: Obj) => o
          case existing =>
            if existing.isDefined then
              reporter.warn(s"key '$key' replaces key '${seg}'", origin)
            val o = Obj(m.LinkedHashMap(), List(origin))
            obj.fields(seg) = o
            o
      val last = segments.last
      obj.fields.get(last) match
        case Some(_: Obj) => reporter.warn(s"key '$key' replaces its sub-keys", origin)
        case Some(_) => reporter.warn(s"duplicate key '$key', the last one takes precedence", origin)
        case None =>
      obj.fields(last) = Str(value, LitKind.Unknown, List(origin))

    // `Properties` calls `put` for every entry, in file order
    val props = new java.util.Properties:
      override def put(k: Object, v: Object): Object =
        set(k.asInstanceOf[String], v.asInstanceOf[String])
        null
    try
      props.load(java.io.InputStreamReader(stream, java.nio.charset.StandardCharsets.UTF_8))
      Some(root)
    catch
      case ex: IllegalArgumentException =>
        reporter.error(ex.getMessage, origin)
        None
