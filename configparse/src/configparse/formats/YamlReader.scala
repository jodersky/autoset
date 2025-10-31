package configparse.formats

import FileReader.Result
import yamlesque.Ctx
import yamlesque.ObjectVisitor
import yamlesque.ArrayVisitor
import yamlesque.Visitor
import configparse.model.{Config, Null, Str, Value, Arr, Origin}

object YamlReader extends FileReader:

  class ValueVisitor(name: String) extends yamlesque.Visitor[Value]:
    vv =>

    class ConfigVisitor(row: Int, col: Int)
        extends yamlesque.ObjectVisitor[Config]:
      val cfg = Config()
      cfg.origins = List(Origin.File(name, row, col))
      var key: String = null

      override def visitKey(ctx: Ctx, key: String): Unit = this.key = key

      override def subVisitor(): Visitor[?] = vv

      override def visitValue(ctx: Ctx, value: Any): Unit =
        cfg.fields(key) = value.asInstanceOf[Value]

      override def visitEnd(): Config = cfg

    class ArrVisitor extends yamlesque.ArrayVisitor[Arr]:
      val arr = Arr()

      override def visitIndex(ctx: Ctx, idx: Int): Unit = ()

      override def subVisitor(): Visitor[?] = vv

      override def visitValue(ctx: Ctx, value: Any): Unit =
        arr.elems += value.asInstanceOf[Value]

      override def visitEnd(): Arr = arr

    override def visitBlockStringLiteral(ctx: Ctx, text: CharSequence) =
      visitString(ctx, text)

    override def visitArray(ctx: Ctx): ArrayVisitor[Value] = ArrVisitor()

    override def visitObject(ctx: Ctx): ObjectVisitor[Value] =
      ConfigVisitor(ctx.pos.line, ctx.pos.line)

    override def visitString(ctx: Ctx, text: CharSequence): Value =
      val str = Str(text.toString)
      str.origins = Origin.File(name, ctx.pos.line, ctx.pos.col) :: Nil
      str

    override def visitQuotedString(ctx: Ctx, text: CharSequence): Value =
      visitString(ctx, text)

    override def visitEmpty(ctx: Ctx): Value =
      val n = Null()
      n.origins = Origin.File(name, ctx.pos.line, ctx.pos.col) :: Nil
      n

    override def visitBlockStringFolded(ctx: Ctx, text: CharSequence): Value =
      visitString(ctx, text)

  override def read(
      name: String,
      stream: java.io.InputStream,
      sizeHint: Int
  ): Result =
    try
      val value =
        yamlesque.Parser(stream, name).parseValue(0, ValueVisitor(name))
      value match
        case c: Config => Result.Success(c)
        case _: Null   => Result.Success(Config())
        case other     => Result.Error("expected object")
    catch
      case ex: yamlesque.ParseException =>
        Result.Error(ex.message, ex.position.line, ex.position.col, ex.line)
