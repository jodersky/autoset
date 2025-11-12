package configparse.derivation

import configparse.model.Path
import configparse.model.Value
import configparse.model.Origin
import configparse.model.Str
import configparse.model.Arr
import configparse.model.Config
import Result.Success
import Result.Error
import collection.mutable as m
import scala.compiletime.ops.string

trait StandardReaders extends ReaderApi:

  given valueReader: Reader[Value] with
    def read(value: Value, path: Path): Result[Value] = Success(value)
  given strReader: Reader[Str] with
    def read(value: Value, path: Path): Result[Str] = value match
      case c: Str   => Success(c)
      case arr: Arr =>
        Error(invalid =
          Seq(Invalid(path, value, "expected a config string, found an array"))
        )
      case cfg: Config =>
        Error(invalid =
          Seq(Invalid(path, value, "expected a config string, found an object"))
        )
  given arrReader: Reader[Arr] with
    def read(value: Value, path: Path): Result[Arr] = value match
      case c: Arr   => Success(c)
      case str: Str =>
        Error(invalid =
          Seq(Invalid(path, value, "expected a config array, found a string"))
        )
      case cfg: Config =>
        Error(invalid =
          Seq(Invalid(path, value, "expected a config array, found an object"))
        )
  given cfgReader: Reader[Config] with
    def read(value: Value, path: Path): Result[Config] = value match
      case c: Config => Success(c)
      case str: Str  =>
        Error(invalid =
          Seq(Invalid(path, value, "expected a config object, found a string"))
        )
      case arr: Arr =>
        Error(invalid =
          Seq(Invalid(path, value, "expected a config object, found an array"))
        )

  given stringReader: Reader[String] with
    def read(value: Value, path: Path): Result[String] =
      strReader.read(value, path).map(_.str)

  given primitiveReader[A](using fs: clip.readers.FromString[A]): Reader[A] with
    def read(cfg: Value, path: Path): Result[A] = stringReader
      .read(cfg, path)
      .flatMap: s =>
        fs.fromString(s) match
          case Right(a) => Success(a)
          case Left(m)  =>
            Error(invalid = Seq(Invalid(path, cfg, m)))

  given pathReader: Reader[os.Path] with
    def read(value: Value, path: Path): Result[os.Path] = stringReader
      .read(value, path)
      .flatMap: s =>
        val root = value.origins match
          case Origin.File(file, _, _) :: _ =>
            os.Path(file, os.pwd) / os.up
          case _ => os.pwd
        try Success(os.Path(s, root))
        catch
          case _: Exception =>
            Error(invalid =
              Seq(Invalid(path, value, s"$s is not a valid file path"))
            )

  given optReader[A](using p: Reader[A]): Reader[Option[A]] =
    (value: Value, path: Path) =>
      value match
        case Str("null") => Success(None)
        case v           => p.read(v, path).map(r => Some(r))

  given mapReader[K, V, M[K, V] <: Iterable[(K, V)]](using
      kr: Reader[K],
      vr: Reader[V],
      factory: collection.Factory[(K, V), M[K, V]]
  ): Reader[M[K, V]] = (v, p) =>
    cfgReader
      .read(v, p)
      .flatMap: cfg =>
        val fields = cfg.fields
        val missing = m.ArrayBuffer.empty[Path]
        val invalid = m.ArrayBuffer.empty[Invalid]
        val items = factory.newBuilder
        for (k, v) <- fields do
          kr.read(Str(k), p) match
            case Success(readKey) =>
              vr.read(v, p / k) match
                case Success(readValue) =>
                  items += readKey -> readValue
                case Error(m, i) =>
                  missing ++= m
                  invalid ++= i
            case Error(m, i) =>
              missing ++= m
              invalid ++= i
        if missing.isEmpty && invalid.isEmpty then Success(items.result())
        else Error(missing.toSeq, invalid.toSeq)

  given colReader[Elem, Col[Elem] <: Iterable[Elem]](using
      elementReader: Reader[Elem],
      factory: collection.Factory[Elem, Col[Elem]]
  ): Reader[Col[Elem]] = (v, p) =>
    arrReader
      .read(v, p)
      .flatMap: arr =>
        val elems = arr.elems
        val missing = m.ArrayBuffer.empty[Path]
        val invalid = m.ArrayBuffer.empty[Invalid]
        val items = factory.newBuilder
        for (elem, idx) <- elems.zipWithIndex do
          elementReader.read(elem, p / idx.toString) match
            case Success(item) => items += item
            case Error(m, i)   =>
              missing ++= m
              invalid ++= i
        if missing.isEmpty && invalid.isEmpty then Success(items.result())
        else Error(missing.toSeq, invalid.toSeq)
