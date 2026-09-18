package autoset.parsers

import autoset.model.*
import collection.mutable as m
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

/** Parses JSON files. The top-level value must be an object.
  *
  * Numbers and booleans are kept as raw literals; their kind is recorded in
  * [[LitKind]].
  */
object JsonParser extends FormatParser:

  private class ValueVisitor(name: String, ls: LocationStream, reporter: Reporter)
      extends ujson.JsVisitor[Value, Value]:
    vv =>

    private def origins(index: Int) = List(ls.origin(name, index))

    private def lit(s: CharSequence, kind: LitKind, index: Int) =
      Str(s.toString, kind, origins(index))

    override def visitJsonableObject(length: Int, index: Int) =
      new ObjVisitor[Value, Obj]:
        val obj = Obj(m.LinkedHashMap(), origins(index))
        var key: String = null
        var keyIndex: Int = -1

        override def visitKey(index: Int) =
          keyIndex = index
          KeyVisitor
        override def visitKeyValue(v: Any): Unit = key = v.asInstanceOf[String]
        override def subVisitor = vv
        override def visitValue(v: Value, index: Int): Unit =
          if obj.fields.contains(key) then
            reporter.warn(
              s"duplicate key '$key', the last one takes precedence",
              ls.origin(name, keyIndex),
              ls.line(keyIndex)
            )
          obj.fields(key) = v
        override def visitEnd(index: Int) = obj

    override def visitArray(length: Int, index: Int) =
      new ArrVisitor[Value, Arr]:
        val arr = Arr(m.ListBuffer(), origins(index))
        override def subVisitor: Visitor[?, ?] = vv
        override def visitValue(v: Value, index: Int): Unit = arr.values += v
        override def visitEnd(index: Int) = arr

    override def visitString(s: CharSequence, index: Int) =
      lit(s, LitKind.String, index)
    override def visitFloat64StringParts(
        s: CharSequence,
        decIndex: Int,
        expIndex: Int,
        index: Int
    ) = lit(s, LitKind.Num, index)
    override def visitTrue(index: Int) = lit("true", LitKind.Bool, index)
    override def visitFalse(index: Int) = lit("false", LitKind.Bool, index)
    override def visitNull(index: Int) = Null(origins(index))

  private object KeyVisitor extends SimpleVisitor[Value, String]:
    def expectedMsg = "expected string"
    override def visitString(s: CharSequence, index: Int) = s.toString

  def parse(
      name: String,
      stream: java.io.InputStream,
      sizeHint: Int,
      reporter: Reporter
  ): Option[Obj] =
    val ls = LocationStream(stream, sizeHint)
    try
      ujson.transform(ls, ValueVisitor(name, ls, reporter)) match
        case o: Obj => Some(o)
        case other =>
          val idx = other.effectiveOrigin match
            case Origin.File(_, idx, _, _, _) => idx
            case _ => 0
          reporter.error("expected a top-level JSON object", ls.origin(name, idx), ls.line(idx))
          None
    catch
      case ex: ujson.ParseException =>
        reporter.error(ex.clue, ls.origin(name, ex.index), ls.line(ex.index))
        None
      case ex: ujson.IncompleteParseException =>
        val end = Int.MaxValue // clamped to the end of the file
        reporter.error("unexpected end of file", ls.origin(name, end), ls.line(end))
        None
