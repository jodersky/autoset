package configparse.derivation

import configparse.model.Path
import configparse.model.Value
import configparse.model.Origin
import configparse.model.Str
import configparse.model.Arr
import configparse.model.Config
import configparse.model.Null
import Result.Success
import Result.Error
import collection.mutable as m

trait StandardReaders extends ReaderApi:

  // given primitiveReader[A](using sr: flags.readers.Reader[A]): Reader[A] with
  //   def read(cfg: Value, path: Path): Result[A] = cfg match
  //     case Str(s) =>
  //       sr.read(s) match
  //         case flags.readers.ReadResult.Success(a) => Success(a)
  //         case flags.readers.ReadResult.Error(m) =>
  //           Error(FieldError.TypeMismatch(path, cfg, sr.typeName))
  //     case _ =>
  //       Error(FieldError.TypeMismatch(path, cfg, sr.typeName))

  given stringReader: Reader[String] with
    def read(value: Value, path: Path): Result[String] = value match
      case Str(s) => Success(s)
      case _ => Error(FieldError.TypeMismatch(path, value, "string"))

  given intReader: Reader[Int] with
    def read(value: Value, path: Path): Result[Int] = value match
      case Str(s) =>
        try
          Success(s.toInt)
        catch
          case _: NumberFormatException =>
            Error(FieldError.TypeMismatch(path, value, "integer"))
      case _ => Error(FieldError.TypeMismatch(path, value, "integer"))

  given pathReader: Reader[os.Path] with
    def read(value: Value, path: Path): Result[os.Path] =
      value match
        case Str(s) =>
          val root = value.origins match
            case Origin.File(file, _, _) :: _ =>
              os.Path(file, os.pwd) / os.up
            case _ => os.pwd
          try
            Success(os.Path(s, root))
          catch
            case _ =>
              Error(FieldError.TypeMismatch(path, value, "path"))
        case other => Error(FieldError.TypeMismatch(path, value, "path"))

  given valueReader: Reader[Value] with
    def read(value: Value, path: Path): Result[Value] = Success(value)

  given cfgReader: Reader[Config] with
    def read(value: Value, path: Path): Result[Config] = value match
      case c: Config => Success(c)
      case other => Error(FieldError.TypeMismatch(path, value, "config object"))

  given arrReader: Reader[Arr] with
    def read(value: Value, path: Path): Result[Arr] = value match
      case c: Arr => Success(c)
      case other => Error(FieldError.TypeMismatch(path, value, "config array"))

  given strReader: Reader[Str] with
    def read(value: Value, path: Path): Result[Str] = value match
      case c: Str => Success(c)
      case other => Error(FieldError.TypeMismatch(path, value, "config string"))

  given optReader[A](using p: Reader[A]): Reader[Option[A]] =
    (value: Value, path: Path) => value match
      case _: Null => Success(None)
      case v => p.read(v, path).map(r => Some(r))

  given mapReader[K, V, M[K, V] <: Iterable[(K, V)]](
    using kr: Reader[K],
    vr: Reader[V],
    factory: collection.Factory[(K, V), M[K, V]]
  ): Reader[M[K, V]] = (v, p) => v match
    case Config(fields) =>
      val errors = m.ArrayBuffer.empty[FieldError]
      val items = factory.newBuilder
      for (k, v) <- fields do
        kr.read(Str(k), p) match
          case Success(readdKey) =>
            vr.read(v, p / k) match
              case Success(readdValue) =>
                items += readdKey -> readdValue
              case Error(errs*) =>
                errors ++= errs
          case Error(errs*) => errors ++= errs
      if errors.isEmpty then Success(items.result())
      else Error(errors.toSeq*)
    case Null() => Success(factory.newBuilder.result())
    case _ => Error(FieldError.TypeMismatch(p, v, "config map"))

  given colReader[Elem, Col[Elem] <: Iterable[Elem]](
    using elementReader: Reader[Elem],
    factory: collection.Factory[Elem, Col[Elem]]
  ): Reader[Col[Elem]] = (v, p) => v match
    case Arr(elems) =>
      val errors = m.ArrayBuffer.empty[FieldError]
      val items = factory.newBuilder
      for (elem, idx) <- elems.zipWithIndex do
        elementReader.read(elem, p / idx.toString) match
          case Success(item) => items += item
          case Error(errs*) => errors ++= errs
      if errors.isEmpty then Success(items.result())
      else Error(errors.toSeq*)
    case Null() => Success(factory.newBuilder.result())
    case _ => Error(FieldError.TypeMismatch(p, v, "config array"))
