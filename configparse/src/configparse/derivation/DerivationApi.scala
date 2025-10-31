package configparse.derivation

import configparse.model.Value
import configparse.model.Config
import configparse.model.Arr
import configparse.model.Path
import configparse.model.Null
import Result.Error
import Result.Success

trait DerivationApi extends ReaderApi:
  api =>

  def scalaNameToConfigName(str: String): String

  case class ProductReader[A](
      fieldNames: Seq[String],
      fieldDefaults: Seq[Option[() => ?]],
      fieldReaders: Seq[Reader[?]],
      instantiate: Seq[?] => A
  ) extends Reader[A]:
    import collection.mutable as m

    def read(value: Value, path: Path): Result[A] = value match
      case cfg: Config => readImpl(cfg, path)
      case cfg: Null   => readImpl(Config(), path)
      case _ => Error(FieldError.TypeMismatch(path, value, "config object"))

    def readImpl(cfg: Config, path: Path): Result[A] =
      val fields = new Array[Any](fieldNames.length)
      val errors = m.ArrayBuffer.empty[FieldError]
      var i = 0
      while i < fields.length do
        val segment = scalaNameToConfigName(fieldNames(i))
        cfg.fields.get(segment) match
          case None if fieldDefaults(i).isDefined =>
            fields(i) = fieldDefaults(i).get()
          case None =>
            // errors += FieldError.Missing(path / segment)
            fieldReaders(i).read(Null(), path / segment) match
              case Result.Success(a)   => fields(i) = a
              case Result.Error(errs*) => errors ++= errs
          case Some(value) =>
            fieldReaders(i).read(value, path / segment) match
              case Result.Success(a)   => fields(i) = a
              case Result.Error(errs*) => errors ++= errs
        i += 1

      if errors.isEmpty then
        try Result.Success(instantiate(fields.toIndexedSeq))
        catch
          case ex: IllegalArgumentException =>
            Error(
              FieldError.RequirementFailed(path, cfg, ex)
            )
      else Result.Error(errors.toSeq*)

  // implicit class Derivable(p: Reader.type)
  extension (p: Reader.type)
    inline def derived[A]: ProductReader[A] =
      derivedImpl[A].asInstanceOf[ProductReader[A]]
    inline def derivedImpl[A] = ${ macros.derivedImpl[api.type, A]('api) }

object macros:
  import scala.quoted.Expr
  import scala.quoted.Quotes
  import scala.quoted.Type

  private def getDefaultParams(using
      qctx: Quotes
  )(method: qctx.reflect.Symbol): Map[qctx.reflect.Symbol, Expr[?]] =
    import qctx.reflect.*
    val pairs = for
      (param, idx) <- method.paramSymss.flatten.zipWithIndex
      if (param.flags.is(Flags.HasDefault))
    yield
      val term = if method.isClassConstructor then
        val defaultName = s"$$lessinit$$greater$$default$$${idx + 1}"
        Ref(method.owner.companionModule.methodMember(defaultName).head)
      else
        val defaultName = s"${method.name}$$default$$${idx + 1}"
        Ref(method.owner.methodMember(defaultName).head)
      param -> term.asExpr
    pairs.toMap

  def derivedImpl[Api <: DerivationApi: Type, A: Type](api: Expr[Api])(using
      qctx: Quotes
  ): Expr[DerivationApi#Reader[A]] =
    import qctx.reflect.*

    val tpe = TypeRepr.of[A]
    if !(tpe <:< TypeRepr.of[Product]) then
      report.error(s"${tpe.show} is not a product type")
      return '{ ??? }

    val constructor =
      tpe.typeSymbol.primaryConstructor // companionModule.methodMember("apply").head

    val fieldNames: List[List[Expr[String]]] =
      for params <- constructor.paramSymss
      yield for param <- params yield Expr(param.name)

    val defaults = getDefaultParams(constructor)
    val fieldDefaults: List[List[Expr[Option[() => ?]]]] =
      for params <- constructor.paramSymss
      yield for param <- params yield defaults.get(param) match
        case None       => '{ None }
        case Some(expr) => '{ Some(() => ${ expr }) }

    val fieldReaders: List[List[Expr[DerivationApi#Reader[?]]]] =
      for params <- constructor.paramSymss yield
        for param <- params yield
          val readerType = TypeSelect(
            api.asTerm,
            "Reader"
          ).tpe.appliedTo(param.termRef.widenTermRefByName)
          Implicits.search(readerType) match
            case iss: ImplicitSearchSuccess =>
              iss.tree.asExprOf[DerivationApi#Reader[?]]
            case _ =>
              report.error(
                s"No ${api.asTerm.tpe.widenTermRefByName.show}.Reader[${param.termRef.widenTermRefByName.show}] available for parameter ${param.name}.",
                param.pos.get
              )
              '{ ??? }
        end for
      end for

    val instantiate = '{ (argss: Seq[?]) =>
      ${
        val base: Term =
          Select(New(TypeTree.ref(constructor.owner)), constructor)
        var i = 0
        val accesses: List[List[Term]] =
          for params <- constructor.paramSymss
          yield for param <- params
          yield param.termRef.widenTermRefByName.asType match
            case '[t] =>
              val expr = '{
                argss(${ Expr(i) }).asInstanceOf[t]
              }
              i += 1
              expr.asTerm
        val application =
          accesses.foldLeft(base)((lhs, args) => Apply(lhs, args))
        application.asExprOf[A]
      }
    }

    '{
      val p = $api
      val names: Seq[String] = ${ Expr.ofSeq(fieldNames.flatten) }
      val defaults: Seq[Option[() => ?]] = ${
        Expr.ofSeq(fieldDefaults.flatten)
      }
      val readers: Seq[DerivationApi#Reader[?]] = ${
        Expr.ofSeq(fieldReaders.flatten)
      }
      p.ProductReader(
        names,
        defaults,
        readers.asInstanceOf[Seq[p.Reader[?]]],
        ${ instantiate }
      )
    }
