package autoset.parsers

import autoset.model.*
import collection.mutable as m
import yamlesque.{ArrayVisitor, Ctx, ObjectVisitor}

/** Parses YAML files, which must contain a single document. The top-level
  * value must be a map, or empty.
  *
  * Plain scalars are typed according to the YAML 1.2 core schema: `true` and
  * `false` are booleans, numbers are kept as written, and `null`, `~` and empty
  * values are null. Everything else, including quoted and block strings, is
  * text.
  */
object YamlParser extends FormatParser:

  private class ValueVisitor(name: String, ls: LocationStream, reporter: Reporter)
      extends yamlesque.Visitor[Value]:
    vv =>

    private def origin(ctx: Ctx) =
      Origin.File(name, ctx.pos.index, ctx.pos.line, ctx.pos.col)

    private def lit(ctx: Ctx, text: CharSequence, kind: LitKind) =
      Str(text.toString, kind, List(origin(ctx)))

    override def visitObject(ctx: Ctx): ObjectVisitor[Value] =
      new ObjectVisitor[Value]:
        val obj = Obj(m.LinkedHashMap(), List(origin(ctx)))
        var key: String = null
        var keyCtx: Ctx = null

        override def visitKey(ctx: Ctx, key: String): Unit =
          this.key = key
          keyCtx = ctx
        override def subVisitor() = vv
        override def visitValue(ctx: Ctx, value: Any): Unit =
          if obj.fields.contains(key) then
            reporter.warn(
              s"duplicate key '$key', the last one takes precedence",
              origin(keyCtx),
              ls.line(keyCtx.pos.index)
            )
          obj.fields(key) = value.asInstanceOf[Value]
        override def visitEnd() = obj

    override def visitArray(ctx: Ctx): ArrayVisitor[Value] =
      new ArrayVisitor[Value]:
        val arr = Arr(m.ListBuffer(), List(origin(ctx)))
        override def visitIndex(ctx: Ctx, idx: Int): Unit = ()
        override def subVisitor() = vv
        override def visitValue(ctx: Ctx, value: Any): Unit =
          arr.values += value.asInstanceOf[Value]
        override def visitEnd() = arr

    override def visitEmpty(ctx: Ctx) = Null(List(origin(ctx)))
    override def visitBool(ctx: Ctx, value: Boolean) =
      lit(ctx, value.toString, LitKind.Bool)
    override def visitNumber(ctx: Ctx, text: CharSequence) = lit(ctx, text, LitKind.Num)
    override def visitString(ctx: Ctx, text: CharSequence) = lit(ctx, text, LitKind.String)
    override def visitQuotedString(ctx: Ctx, text: CharSequence) =
      lit(ctx, text, LitKind.String)
    override def visitBlockStringLiteral(ctx: Ctx, text: CharSequence) =
      lit(ctx, text, LitKind.String)
    override def visitBlockStringFolded(ctx: Ctx, text: CharSequence) =
      lit(ctx, text, LitKind.String)

  def parse(
      name: String,
      stream: java.io.InputStream,
      sizeHint: Int,
      reporter: Reporter
  ): Option[Obj] =
    val ls = LocationStream(stream, sizeHint)
    try
      yamlesque.Parser(ls, name).parseSingleDocument(ValueVisitor(name, ls, reporter)) match
        case o: Obj => Some(o)
        // an empty file, e.g. one where everything is commented out
        case _: Null => Some(Obj(m.LinkedHashMap(), List(Origin.File(name, 0, 1, 1))))
        case other =>
          val pos = other.effectiveOrigin.asInstanceOf[Origin.File]
          reporter.error("expected a top-level YAML map", pos, ls.line(pos.idx))
          None
    catch
      case ex: yamlesque.ParseException =>
        ls.drain()
        val pos = ex.position
        reporter.error(
          ex.message,
          Origin.File(name, pos.index, pos.line, pos.col),
          ls.line(pos.index)
        )
        None
