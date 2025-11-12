package configparse.formats

import FileReader.Result
import upickle.core.Visitor
import configparse.util.TextUtils
import java.io.InputStream
import configparse.model.{Config, Str, Value, Arr, Origin}

object JsonReader extends FileReader:

  class ValueVisitor(name: String, ls: TextUtils.LocationStream)
      extends ujson.JsVisitor[Value, Value]:
    vv =>

    class ConfigVisitor(idx: Int)
        extends upickle.core.ObjVisitor[Value, Config]:
      val cfg = Config()
      val (row, col) = ls.posToRowAndCol(idx)
      cfg.origins = List(Origin.File(name, row, col))
      var key: String = null

      override def visitKeyValue(v: Any): Unit =
        key = v.asInstanceOf[String]

      override def visitEnd(index: Int): Config = cfg

      override def subVisitor = vv
      override def visitValue(v: Value, index: Int): Unit =
        cfg.fields(key) = v

      override def visitKey(index: Int) =
        KeyVisitor

    object KeyVisitor extends upickle.core.SimpleVisitor[Value, String]:
      def expectedMsg: String = "expected string"
      override def visitString(s: CharSequence, index: Int): String =
        s.toString()

    class ArrVisitor(idx: Int) extends upickle.core.ArrVisitor[Value, Arr]:
      val arr = Arr()

      override def subVisitor: Visitor[?, ?] = vv

      override def visitValue(v: Value, index: Int): Unit =
        arr.elems += v

      override def visitEnd(index: Int): Arr = arr

    override def visitTrue(index: Int): Value = visitString("true", index)

    override def visitJsonableObject(length: Int, index: Int) =
      ConfigVisitor(index)

    override def visitFloat64StringParts(
        s: CharSequence,
        decIndex: Int,
        expIndex: Int,
        index: Int
    ): Value = visitString(s, index)

    override def visitString(s: CharSequence, index: Int): Value =
      val v = Str(s.toString)
      val (rows, cols) = ls.posToRowAndCol(index)
      v.origins = Origin.File(name, rows, cols) :: Nil
      v

    override def visitArray(length: Int, index: Int) = ArrVisitor(index)

    override def visitNull(index: Int): Value = visitString("null", index)

    override def visitFalse(index: Int): Value = visitString("false", index)

  override def read(name: String, stream: InputStream, sizeHint: Int): Result =
    val ls = TextUtils.LocationStream(stream, sizeHint)
    try
      ujson.transform(ls, ValueVisitor(name, ls)) match
        case o: Config => Result.Success(o)
        case other     =>
          Result.Error("JSON file does not contain a top-level object.")
    catch
      case ex: ujson.ParseException =>
        val (row, col) = ls.posToRowAndCol(ex.index)
        Result.Error(ex.clue, row, col)
      case ex: ujson.IncompleteParseException =>
        Result.Error(
          "reached end of file before the top-level JSON object was closed"
        )
