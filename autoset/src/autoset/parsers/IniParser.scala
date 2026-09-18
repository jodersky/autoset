package autoset.parsers

import autoset.model.*
import collection.mutable as m
import configparse.ini

/** Parses INI files.
  *
  * Sections are dotted paths to nested objects (`[a.b]`), and may be reopened
  * later in the file. Values are plain text up to the end of the line; since
  * INI is untyped, their kind is [[LitKind.Unknown]]. Keys without a value are
  * ignored.
  */
object IniParser extends FormatParser:

  private class Visitor(name: String, ls: LocationStream, reporter: Reporter, root: Obj)
      extends ini.Visitor:
    var section = root
    var key: String = null
    var keyPos: ini.Pos = null

    private def origin(pos: ini.Pos) = ls.origin(name, pos.idx)
    private def warn(message: String, pos: ini.Pos) =
      reporter.warn(message, origin(pos), ls.line(pos.idx))

    override def visitKey(pos: ini.Pos, key: String): Unit =
      this.key = key
      keyPos = pos

    override def visitString(pos: ini.Pos, text: String): Unit =
      if section.fields.contains(key) then
        warn(s"duplicate key '$key', the last one takes precedence", keyPos)
      section.fields(key) = Str(text, LitKind.Unknown, List(origin(pos)))

    override def visitEmpty(pos: ini.Pos): Unit = ()

    override def visitSection(pos: ini.Pos, sectionKey: Seq[String]): Unit =
      var obj = root
      for seg <- sectionKey do
        obj = obj.fields.get(seg) match
          case Some(o: Obj) =>
            o.origins = origin(pos) :: o.origins
            o
          case existing =>
            if existing.isDefined then
              warn(s"section [${sectionKey.mkString(".")}] replaces key '$seg'", pos)
            val o = Obj(m.LinkedHashMap(), List(origin(pos)))
            obj.fields(seg) = o
            o
      section = obj

  def parse(
      name: String,
      stream: java.io.InputStream,
      sizeHint: Int,
      reporter: Reporter
  ): Option[Obj] =
    val ls = LocationStream(stream, sizeHint)
    val root = Obj(m.LinkedHashMap(), List(Origin.File(name, 0, 1, 1)))
    try
      ini.Parser(ls, Visitor(name, ls, reporter, root)).parse()
      Some(root)
    catch
      case ex: ini.ParseException =>
        reporter.error(ex.message, ls.origin(name, ex.pos.idx), ls.line(ex.pos.idx))
        None
