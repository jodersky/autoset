package configparse.formats

import FileReader.Result
import configparse.ini
import configparse.ini.Pos
import java.io.InputStream
import configparse.model.{Config, Null, Str, Value, Arr, Origin}

object IniReader extends FileReader:

  class Visitor(name: String, cfg: Config) extends ini.Visitor:
    var active = cfg
    var key: String = null

    override def visitKey(pos: Pos, key: String): Unit = this.key = key

    override def visitString(pos: Pos, text: String): Unit =
      val v = Str(text)
      v.origins = Origin.File(name, pos.row, pos.col) :: Nil
      active.fields(key) = v

    override def visitEmpty(pos: Pos): Unit =
      val v = Null()
      v.origins = Origin.File(name, pos.row, pos.col) :: Nil
      active.fields(key) = v

    override def visitSection(pos: Pos, sectionKey: Seq[String]): Unit =
      var cfg1 = cfg
      for seg <- sectionKey do
        cfg1.fields.get(seg) match
          case Some(c: Config) =>
            cfg1 = c
          case _ =>
            val c = Config()
            c.origins = List(Origin.File(name, pos.row, pos.col))
            cfg1.fields(seg) = c
            cfg1 = c
      active = cfg1

  override def read(name: String, stream: InputStream, sizeHint: Int): Result =
    val cfg = Config()
    try
      ini.Parser(stream, Visitor(name, cfg)).parse()
      Result.Success(cfg)
    catch
      case ex: ini.ParseException =>
        Result.Error(ex.message, ex.pos.row, ex.pos.col, ex.line)
