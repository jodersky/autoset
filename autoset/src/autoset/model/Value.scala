package autoset.model

import collection.mutable as m

sealed trait Value:
  /** Origins of this value and of the values it replaced at the same key, most
    * recent first. Never empty.
    *
    * For objects, this lists every source that defined the object; merged-in
    * fields carry their own origins.
    */
  var origins: List[Origin]

  /** Where this value was last defined. */
  def effectiveOrigin: Origin = origins.head

  /** Render this value in a JSON-like format, for debugging. Keys are only
    * quoted when necessary, literals are always quoted, and origins are shown
    * as `//` comments.
    *
    * By default, origins are only shown where they add information: each
    * container is labelled with the source (e.g. file) most of its children
    * come from, and children from that same source are left unannotated.
    * Values that were overridden, or that come from a named source such as an
    * environment variable, are always annotated. Pass `verbose = true` to
    * annotate every value with all of its origins.
    */
  def pretty(verbose: Boolean = false): String =
    val sb = StringBuilder()
    Value.Printer(sb, verbose).write(this, 0, "", None)
    sb.result()

object Value:

  /** The source an origin belongs to, if its children may be summarized by
    * that source. Named origins (env, props, args) have none, since their name
    * is the useful part.
    */
  private def source(o: Origin): Option[String] = o match
    case Origin.File(path, _, _, _, _) => Some(path)
    case Origin.Code(path, _, _, _) => Some(s"code $path")
    case Origin.Default => Some("default")
    case _ => None

  /** The most common source of a value's leaves; on ties, the first one. */
  private def mainSource(v: Value): Option[String] = v match
    case o: Obj if o.fields.nonEmpty => dominant(o.fields.values)
    case a: Arr if a.values.nonEmpty => dominant(a.values)
    case _ => source(v.effectiveOrigin)

  private def dominant(vs: Iterable[Value]): Option[String] =
    val srcs = vs.flatMap(mainSource).toSeq
    srcs.distinct.maxByOption(s => srcs.count(_ == s))

  private def describe(origins: List[Origin]): String =
    val overrides =
      if origins.tail.isEmpty then ""
      else origins.tail.map(_.pretty).mkString(" (overrides ", ", ", ")")
    origins.head.pretty + overrides

  private class Printer(sb: StringBuilder, verbose: Boolean):

    /** @param ctx the source of the enclosing container, if any */
    def write(v: Value, indent: Int, sep: String, ctx: Option[String]): Unit =
      val items: Seq[(Option[String], Value)] = v match
        case o: Obj => o.fields.toSeq.map((k, c) => (Some(k), c))
        case a: Arr => a.values.toSeq.map((None, _))
        case _ => Nil
      if items.isEmpty then sb ++= leafText(v) ++= sep ++= comment(leafComment(v, ctx))
      else
        val inner = if verbose then None else dominant(items.map(_._2)).orElse(ctx)
        // a merged object's origins are contributors, not overrides
        val header = v match
          // arrays aren't merged, so any further origins are overrides
          case _: Arr if !verbose && v.origins.tail.nonEmpty => Some(describe(v.origins))
          case _ if !verbose => inner.filter(!ctx.contains(_))
          case _: Obj => Some(v.origins.map(_.pretty).mkString(", "))
          case _ => Some(describe(v.origins))
        val (open, close) = if v.isInstanceOf[Obj] then ("{", "}") else ("[", "]")
        val oneLine = Option.when(
          !verbose && v.isInstanceOf[Arr] && items.forall((_, c) =>
            (c.isInstanceOf[Str] || c.isInstanceOf[Null]) && leafComment(c, inner).isEmpty
          )
        )(items.map((_, c) => leafText(c)).mkString(open, ", ", close)).filter(_.length <= 60)
        oneLine match
          case Some(line) => sb ++= line ++= sep ++= comment(header)
          case None =>
            sb ++= open ++= comment(header) ++= "\n"
            for ((k, c), i) <- items.zipWithIndex do
              sb ++= "  " * (indent + 1)
              k.foreach(k => sb ++= key(k) ++= ": ")
              write(c, indent + 1, if i < items.size - 1 then "," else "", inner)
              sb ++= "\n"
            sb ++= "  " * indent ++= close ++= sep

    private def leafText(v: Value): String = v match
      case s: Str => quote(s.raw)
      case _: Null => "null"
      case _: Obj => "{}"
      case _: Arr => "[]"

    private def leafComment(v: Value, ctx: Option[String]): Option[String] =
      v.origins match
        case List(o) if !verbose && source(o).isDefined && source(o) == ctx => None
        case os => Some(describe(os))

    private def comment(text: Option[String]): String = text.fold("")(" // " + _)

  private def key(k: String): String =
    if k.nonEmpty && k.forall(c => c.isLetterOrDigit || c == '_' || c == '-') then k
    else quote(k)

  private def quote(s: String): String =
    val sb = StringBuilder("\"")
    s.foreach {
      case '"' => sb ++= "\\\""
      case '\\' => sb ++= "\\\\"
      case '\n' => sb ++= "\\n"
      case '\r' => sb ++= "\\r"
      case '\t' => sb ++= "\\t"
      case c => sb += c
    }
    sb += '"'
    sb.result()

case class Obj(
    fields: m.LinkedHashMap[String, Value],
    var origins: List[Origin]
) extends Value

case class Arr(
    values: m.ListBuffer[Value],
    var origins: List[Origin]
) extends Value
case class Str(
    raw: String,
    kind: LitKind,
    var origins: List[Origin]
) extends Value
case class Null(var origins: List[Origin]) extends Value

enum LitKind:
  case String
  case Num
  case Bool
  case Unknown

enum Origin:
  // `idx` is the byte offset in the source (UTF-8), used to look up text.
  // `line` and `col` are 1-based and only used for display; `col` counts
  // characters, not bytes.
  // Any of them is -1 when unknown, since not all formats report positions.
  // `path` is for display, and may be relative; `absolute` is the file's
  // absolute path, if known (it is set for files loaded with `Api.load`).
  case File(path: String, idx: Int, line: Int, col: Int, absolute: Option[String] = None)
  case Env(name: String)
  case Props(name: String)
  case Arg()
  case Code(path: String, idx: Int, line: Int, col: Int) // set un user code
  case Default // from the case class parameter

  def pretty: String = this match
    case File(path, _, line, col, _) => Origin.location(path, line, col)
    case Env(name) => s"env $name"
    case Props(name) => s"prop $name"
    case Arg() => "arg"
    case Code(path, _, line, col) => "code " + Origin.location(path, line, col)
    case Default => "default"

object Origin:
  /** `path:line:col`, leaving out unknown parts. */
  private def location(path: String, line: Int, col: Int): String =
    if line <= 0 then path
    else if col <= 0 then s"$path:$line"
    else s"$path:$line:$col"
