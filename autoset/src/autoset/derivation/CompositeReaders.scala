package autoset.derivation

import autoset.model.*
import ReaderUtils.mismatch

/** Readers for collections, built from the readers of their elements. */
trait CompositeReaders extends ReadersApi:
  import scala.collection.Factory
  import scala.collection.mutable as m
  import scala.reflect.ClassTag

  /** Reads `null` as `None`, and anything else with the reader for `A`.
    *
    * Case class fields of type `Option` are also `None` when missing.
    */
  given OptionReader[A](using elem: Reader[A]): Reader[Option[A]] with
    def read(value: Value, path: Vector[String], reporter: Reporter) =
      value match
        case _: Null => Some((None, value))
        case _ => elem.read(value, path, reporter).map((a, shown) => (Some(a), shown))

  /** Reads an array into any iterable collection that has a `Factory`, e.g.
    * `List`, `Vector`, `Set` or `mutable.ArrayBuffer`.
    *
    * Untyped strings (from sources without arrays, such as environment
    * variables, system properties, INI and properties files) are split on
    * commas instead, e.g. `APP_HOSTS=a, b` is read as `["a", "b"]`. Items are
    * trimmed, an empty string is an empty collection, and there is no way to
    * escape a comma.
    *
    * Every element is read, so that all errors are reported; the result is
    * `None` if any of them failed. Elements are at path `<path>.<index>`.
    */
  given IterableReader[C[X] <: Iterable[X], A](using
      factory: Factory[A, C[A]],
      elem: Reader[A]
  ): Reader[C[A]] with
    def read(value: Value, path: Vector[String], reporter: Reporter) =
      value match
        case arr: Arr => readAll(arr.values, arr.origins, path, reporter)
        case Str(raw, LitKind.Unknown, origins) =>
          val items =
            if raw.trim.isEmpty then Nil
            else raw.split(",", -1).toList.map(item => Str(item.trim, LitKind.Unknown, origins))
          readAll(items, origins, path, reporter)
        case _ => mismatch("an array", value, path, reporter)

    private def readAll(
        values: Iterable[Value],
        origins: List[Origin],
        path: Vector[String],
        reporter: Reporter
    ): Option[(C[A], Value)] =
      val results =
        values.zipWithIndex.map((v, i) => elem.read(v, path :+ i.toString, reporter))
      if results.exists(_.isEmpty) then None
      else
        val builder = factory.newBuilder
        val shown = m.ListBuffer.empty[Value]
        for case Some((a, v)) <- results do
          builder += a
          shown += v
        Some((builder.result(), Arr(shown, origins)))

  /** Reads an object into any map with string keys that has a `Factory`, e.g.
    * `Map`, `SortedMap` or `mutable.Map`.
    *
    * Every field is read, so that all errors are reported; the result is
    * `None` if any of them failed. Fields are at path `<path>.<key>`.
    */
  given MapReader[M[K, V] <: collection.Map[K, V], A](using
      factory: Factory[(String, A), M[String, A]],
      elem: Reader[A]
  ): Reader[M[String, A]] with
    def read(value: Value, path: Vector[String], reporter: Reporter) =
      value match
        case obj: Obj =>
          val results = obj.fields.toSeq.map((k, v) => k -> elem.read(v, path :+ k, reporter))
          if results.exists(_._2.isEmpty) then None
          else
            val builder = factory.newBuilder
            val shown = m.LinkedHashMap.empty[String, Value]
            for case (k, Some((a, v))) <- results do
              builder += k -> a
              shown(k) = v
            Some((builder.result(), Obj(shown, obj.origins)))
        case _ => mismatch("an object", value, path, reporter)

  /** Reads an array like any other collection (see `IterableReader`). */
  given ArrayReader[A](using ClassTag[A], Reader[A]): Reader[Array[A]] with
    def read(value: Value, path: Vector[String], reporter: Reporter) =
      summon[Reader[Vector[A]]].read(value, path, reporter).map((v, shown) => (v.toArray, shown))
