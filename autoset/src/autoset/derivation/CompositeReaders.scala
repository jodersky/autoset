package autoset.derivation

import autoset.model.*
import ReaderUtils.mismatch
import scala.collection.mutable as m

/** Readers for collections, built from the readers of their elements. */
trait CompositeReaders extends ReadersApi:
  import scala.collection.Factory
  import scala.reflect.ClassTag

  /** Reads `null` as `None`, and anything else with the reader for `A`.
    *
    * A case class field of this type is not optional by virtue of being an
    * `Option`: it must be set, possibly to `null`. Give it a default, e.g.
    * `nick: Option[String] = None`, for a field which may be left out.
    */
  given OptionReader[A](using elem: Reader[A]): Reader[Option[A]] with
    override def show(a: Option[A]) = Some(a.flatMap(elem.show).getOrElse(Null(Nil)))
    def read(value: Value, base: Option[Option[A]], ctx: Context) =
      value match
        case _: Null => Some(None)
        // a present value falls back to what the base has inside its option
        case _ => elem.read(value, base.flatten, ctx).map(Some(_))

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
    override def show(a: C[A]) =
      val items = a.map(elem.show)
      Option.when(items.forall(_.isDefined))(Arr(items.flatten.to(m.ListBuffer), Nil))
    // lists are never merged, so a list which is set replaces `base` entirely
    def read(value: Value, base: Option[C[A]], ctx: Context) =
      value match
        case arr: Arr => readAll(arr.values, ctx)
        case str @ Str(raw, LitKind.Unknown, origins) =>
          val items =
            if raw.trim.isEmpty then Nil
            else
              for item <- raw.split(",", -1).toList yield
                val s = Str(item.trim, LitKind.Unknown, origins)
                s.secret = str.secret
                s
          readAll(items, ctx)
        case _ => mismatch("an array", value, ctx)

    private def readAll(values: Iterable[Value], ctx: Context): Option[C[A]] =
      val results = values.zipWithIndex.map((v, i) => elem.read(v, None, ctx / i))
      if results.exists(_.isEmpty) then None
      else Some(results.flatten.to(factory))

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
    override def show(a: M[String, A]) =
      val items = a.toSeq.map((k, v) => k -> elem.show(v))
      Option.when(items.forall(_._2.isDefined))(
        Obj(items.map((k, v) => k -> v.get).to(m.LinkedHashMap), Nil)
      )
    // a value at a key falls back to the base's value at the same key
    def read(value: Value, base: Option[M[String, A]], ctx: Context) =
      value match
        case obj: Obj =>
          val results =
            obj.fields.toSeq.map((k, v) => k -> elem.read(v, base.flatMap(_.get(k)), ctx / k))
          if results.exists(_._2.isEmpty) then None
          else Some(results.map((k, a) => k -> a.get).to(factory))
        case _ => mismatch("an object", value, ctx)

  /** Reads an array like any other collection (see `IterableReader`). */
  given ArrayReader[A](using ClassTag[A], Reader[A]): Reader[Array[A]] with
    override def show(a: Array[A]) = summon[Reader[Vector[A]]].show(a.toVector)
    def read(value: Value, base: Option[Array[A]], ctx: Context) =
      summon[Reader[Vector[A]]].read(value, base.map(_.toVector), ctx).map(_.toArray)
