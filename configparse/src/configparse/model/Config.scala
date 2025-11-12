package configparse.model

import collection.mutable
import configparse.util.SourcePos

sealed trait Value:

  /** All origins that were used to get this value. */
  var origins: List[Origin] = Nil

  def flatten(path: Path = Path.Empty): mutable.LinkedHashMap[String, Str] =
    val buffer = mutable.LinkedHashMap.empty[String, Str]
    flattenInto(path, buffer)
    buffer

  def flattenInto(
      path: Path = Path.Empty,
      buffer: mutable.LinkedHashMap[String, Str]
  ): Unit =
    this match
      case t: Str =>
        buffer += path.segments.mkString(".") -> t
      case arr: Arr =>
        for (elem, idx) <- arr.elems.zipWithIndex do
          elem.flattenInto(path / idx.toString, buffer)
      case cfg: Config =>
        for (key, value) <- cfg.fields do value.flattenInto(path / key, buffer)

  def dumpInto(
      path: Path = Path.Empty,
      out: java.io.PrintStream = System.err
  ): Unit =
    for (key, str) <- flatten(path).toSeq.sortBy(_._1) do
      out.print(key)
      out.println(s"='${str.str}'")
      out.print("  from ")

      str.origins match
        case Nil       => out.println("<unknown>")
        case head :: _ => out.println(head.pretty)

  def dump(path: Path = Path.Empty): String =
    val baos = java.io.ByteArrayOutputStream()
    dumpInto(path, java.io.PrintStream(baos))
    new String(baos.toByteArray(), "utf-8")

/** A text value. */
case class Str(str: String) extends Value

/** An array of configuration values. */
case class Arr(elems: mutable.ArrayBuffer[Value] = mutable.ArrayBuffer.empty)
    extends Value

/** An object mapping string keys to configuration values. */
case class Config(
    fields: mutable.LinkedHashMap[String, Value] = mutable.LinkedHashMap.empty
) extends Value:

  /** Merge the fields from the given object into this one.
    *
    * Note that only objects are merged. Any other values will overwrite
    * existing ones.
    */
  def mergeFrom(other: Config): Unit =
    for (k, v) <- other.fields do
      if !fields.contains(k) then fields += k -> v
      else
        val lhs = fields(k)
        (lhs, v) match
          case (o1: Config, o2: Config) =>
            o1.mergeFrom(o2)
            o1.origins = o2.origins ::: o1.origins
          case (_, o2: Config) if o2.fields.isEmpty =>
          // don't overwrite with empty config
          case _ =>
            v.origins = v.origins ::: lhs.origins
            fields(k) = v
      end if

  def setValue(path: Path, value: Value): Unit =
    path.segments.toList match
      case Nil        => sys.error("empty key is not allowed")
      case key :: Nil =>
        (fields.get(key), value) match
          case (Some(t1: Str), t2: Str) =>
            t2.origins = t2.origins ::: t1.origins
          case _ =>
        fields(key) = value
      case head :: tail =>
        val next = fields.get(head) match
          case Some(o: Config) => o
          case _               =>
            val o = Config()
            fields(head) = o
            o
        next.setValue(tail, value)

  def getValue(path: Path): Option[Value] =
    path.segments.toList match
      case Nil         => sys.error("empty key is not allowed")
      case head :: Nil =>
        fields.get(head)
      case head :: tail =>
        fields.get(head) match
          case Some(o: Config) => o.getValue(tail)
          case Some(a: Arr)    =>
            head.toIntOption match
              case Some(idx) if 0 <= idx && idx < a.elems.size =>
                Some(a.elems(idx))
              case _ => None
          case _ => None

  def set(path: Path, value: String, origin: Origin = null)(using
      here: SourcePos
  ): Unit =
    val origin1 =
      if origin != null then origin
      else Origin.Code(here.path, here.row, here.col)
    val v = Str(value)
    v.origins = List(origin1)
    setValue(path, v)

  def get(path: Path): Option[String] =
    getValue(path) match
      case Some(s: Str) => Some(s.str)
      case _            => None

  def remove(path: Path): Unit =
    path.segments.toList match
      case Nil          => sys.error("empty key is not allowed")
      case key :: Nil   => fields.remove(key)
      case head :: tail =>
        fields.get(head) match
          case Some(o: Config) => o.remove(tail)
          case _               => ()

object Config:
  def apply(items: (String, Value)*): Config =
    val map = mutable.LinkedHashMap[String, Value]()
    for (i <- items) map.put(i._1, i._2)
    Config(map)
