package autoset.derivation

import autoset.model.*
import scala.collection.mutable as m
import scala.quoted.*

trait DerivedReaders extends ReadersApi:

  /** The key in config objects of a case class field called `name`.
    *
    * Override this to use a different naming convention in config files
    * than in Scala code, for example snake_case with
    * `ReaderUtils.snakify(name)` or kebab-case with
    * `ReaderUtils.kebabify(name)`. By default, keys are the same as field names.
    */
  def fieldName(name: String): String = name

  /** The name in config files of a case of a sealed type or enum called
    * `name`. By default, names are in lowerCamelCase, like field names, e.g.
    * `Postgres` is `postgres` and `InMemory` is `inMemory`.
    *
    * Override this to use a different naming convention, for example
    * `ReaderUtils.kebabify(name)`, or `name` to use the Scala names.
    */
  def caseName(name: String): String = ReaderUtils.lowerCamelCase(name)

  /** The key in config objects which says which case of a sealed type or enum
    * an object is.
    */
  def discriminator: String = "type"

  /** Derive a reader for `A`, which is one of:
    *
    *   - A case class. The reader expects an object, and reads each field with
    *     the reader for its type, which must be available as a given (except
    *     for unions of string literals, whose reader is derived). A field
    *     that is missing from the object takes its default value if it has
    *     one, is `None` if it is an `Option`, and is an error otherwise. Keys
    *     of the object that don't match any field are warned about, and
    *     marked as unknown. The values of fields annotated with `@secret` are
    *     marked as secret, so that they are never shown, e.g. by `pretty` or
    *     in errors.
    *
    *   - A sealed trait or enum, whose cases are case classes or singletons
    *     (case objects or enum cases without parameters). The reader expects
    *     an object with a `discriminator` key, whose value is the name of a
    *     case, and reads the rest of the object as that case. A string is
    *     short for an object with only the discriminator, so that singletons
    *     can be written as their name. Nested sealed types are flattened to
    *     their cases.
    *
    *   - A union of string literal types, such as `"debug" | "info"`. The
    *     reader expects one of the strings.
    *
    * For example:
    * ```scala
    * case class Db(host: String, port: Int = 5432, @secret password: String)
    * given Reader[Db] = readerFor[Db]
    *
    * enum Level:
    *   case Debug, Info, Warn // `level: info`
    * given Reader[Level] = readerFor[Level]
    *
    * sealed trait Storage
    * case class Disk(path: os.Path) extends Storage // `storage: {type: disk, path: /data}`
    * case object Memory extends Storage // `storage: memory`
    * given Reader[Storage] = readerFor[Storage]
    * ```
    */
  inline def readerFor[A]: Reader[A] = ${ DerivedReaders.readerForImpl[A, Reader[A]]('this) }

  /** Derive a reader for `A` in a `derives` clause. This is the same as
    * `readerFor[A]`:
    *
    * ```scala
    * case class Db(host: String, port: Int = 5432) derives autoset.Reader
    * ```
    */
  extension (companion: Reader.type)
    inline def derived[A]: Reader[A] =
      ${ DerivedReaders.readerForImpl[A, Reader[A]]('this) }

object DerivedReaders:

  /** Prints types without their package, for error messages. */
  private def short(using q: Quotes): q.reflect.Printer[q.reflect.TypeRepr] =
    q.reflect.Printer.TypeReprShortCode

  /** Generate a reader for `A`.
    *
    * `R` is `api.Reader[A]`. It is passed in since the type of the result
    * depends on `api`, which can't be expressed in this method's signature.
    *
    * For a case class defined as:
    *
    * ```scala
    * case class Foo(a: Int, @secret b: String = "x")
    * ```
    *
    * the generated reader is equivalent to:
    * ```scala
    * new api.Reader[Foo]:
    *   def read(value: Value, path: Vector[String], reporter: Reporter) =
    *     value match
    *       case obj: Obj =>
    *         var ok = true
    *
    *         val nameA = api.fieldName("a")
    *         var a: Int = 0
    *         obj.fields.get(nameA) match
    *           case Some(v) =>
    *             summon[api.Reader[Int]].read(v, path :+ nameA, reporter) match
    *               case Some(x) => a = x
    *               case None => ok = false
    *           case None =>
    *             ReaderUtils.missingField(obj, path :+ nameA, reporter)
    *             ok = false
    *
    *         val nameB = api.fieldName("b")
    *         var b: String = null
    *         obj.fields.get(nameB) match
    *           case Some(v) =>
    *             v.markSecret() // before reading, so that errors don't show it
    *             summon[api.Reader[String]].read(v, path :+ nameB, reporter) match
    *               case Some(x) => b = x
    *               case None => ok = false
    *           case None => b = Foo.$lessinit$greater$default$2
    *
    *         for (key, v) <- obj.fields if !List(nameA, nameB).contains(key) do
    *           v.unknown = true
    *           reporter.warn("unknown key '" + ... + "'", v.effectiveOrigin)
    *
    *         if ok then Some(new Foo(a, b)) else None
    *       case _ => ReaderUtils.mismatch("an object", value, path, reporter)
    * ```
    *
    * For a sealed type with cases `Foo` and `Bar`, the reader looks up the
    * name of the case at `api.discriminator`, compares it to
    * `api.caseName("Foo")` and `api.caseName("Bar")`, and reads the object
    * as the matching case, like above.
    *
    * `api` is the instance of `DerivedReaders` that `readerFor` or
    * `Reader.derived` is called on, so that readers are looked up on it. This
    * relies on `api` having a singleton type, which is the case for the
    * `this` of an inline method with a stable prefix.
    */
  def readerForImpl[A: Type, R: Type](api: Expr[DerivedReaders])(using q: Quotes): Expr[R] =
    import q.reflect.*

    val tpe = TypeRepr.of[A]
    val prefix = api.asTerm.tpe
    val readerSym = TypeRepr.of[ReadersApi].typeSymbol.typeMember("Reader")
    def readerOf(t: TypeRepr) = prefix.select(readerSym).appliedTo(t)

    def fail(message: String): Nothing =
      report.errorAndAbort(s"cannot derive a reader for ${tpe.show(using short)}, $message")

    /** What the macro needs to know about a field of a case class.
      *
      * @param reader
      *   the reader for the field's type
      * @param default
      *   the field's default value, if it has one
      * @param annotations
      *   the annotations of the field, e.g. `@secret`
      */
    case class FieldInfo(
        name: String,
        tpe: TypeRepr,
        reader: Term,
        default: Option[Term],
        annotations: List[Term]
    ):
      /** The annotations of type `T`. */
      def annotated[T: Type]: List[Term] = annotations.filter(_.tpe <:< TypeRepr.of[T])

      /** The key given with `@name`, if any. */
      val key: Option[String] = annotated[autoset.name].flatMap(stringArgs).headOption

      /** The keys given with `@deprecatedNames`. */
      val deprecated: List[String] = annotated[autoset.deprecatedNames].flatMap(stringArgs)

      def has[T: Type]: Boolean = annotations.exists(_.tpe <:< TypeRepr.of[T])

    /** The arguments of the annotation `annot`, which must be string literals. */
    def stringArgs(annot: Term): List[String] =
      def strings(t: Term): List[String] = t match
        case Literal(StringConstant(s)) => List(s)
        case Typed(e, _) => strings(e)
        case Repeated(es, _) => es.flatMap(strings)
        case NamedArg(_, e) => strings(e)
        case Inlined(_, Nil, e) => strings(e)
        case _ => report.errorAndAbort(s"expected a string literal in @${annot.tpe.typeSymbol.name}", t.pos)
      annot match
        case Apply(_, args) => args.flatMap(strings)
        case _ => Nil

    /** The argument of a `@readWith` annotation, checked to read `fieldTpe`. */
    def readWithArg(annot: Term, field: String, cls: TypeRepr, fieldTpe: TypeRepr): Term =
      val arg = annot match
        case Apply(_, List(NamedArg(_, arg))) => arg
        case Apply(_, List(arg)) => arg
        case _ => report.errorAndAbort("unexpected @readWith annotation", annot.pos)
      arg.tpe.widen.baseType(readerSym) match
        case AppliedType(_, List(t)) if t =:= fieldTpe => arg
        case AppliedType(_, List(t)) =>
          report.errorAndAbort(
            s"the reader given with @readWith for field '$field' of ${cls.show(using short)} " +
              s"reads ${t.show(using short)}, not ${fieldTpe.show(using short)}",
            arg.pos
          )
        case _ => report.errorAndAbort("@readWith expects a reader", arg.pos)

    /** The fields of the case class `cls`. */
    def fieldsOf(cls: TypeRepr): List[FieldInfo] =
      val sym = cls.typeSymbol
      val params = sym.primaryConstructor.paramSymss.filter(_.forall(_.isTerm)) match
        case params :: Nil => params
        case Nil => Nil
        case _ => fail(s"since ${cls.show(using short)} has more than one parameter list")

      val fields = for ((param, field), i) <- params.zip(sym.caseFields).zipWithIndex yield
        val fieldTpe = cls.memberType(field)

        val readWith = param.annotations.find(_.tpe <:< TypeRepr.of[autoset.readWith])
        val reader = readWith.map(readWithArg(_, param.name, cls, fieldTpe)).getOrElse {
          Implicits.search(readerOf(fieldTpe)) match
            case success: ImplicitSearchSuccess => success.tree
            // a union of string literals written as the field's type has no
            // given reader, so derive one here
            case _: ImplicitSearchFailure if literals(fieldTpe).isDefined =>
              (fieldTpe.asType, readerOf(fieldTpe).asType) match
                case ('[t], '[r]) => readerForImpl[t, r](api).asTerm
            case failure: ImplicitSearchFailure =>
              report.errorAndAbort(
                s"no given instance of Reader[${fieldTpe.show(using short)}] found for field " +
                  s"'${param.name}' of ${cls.show(using short)}: ${failure.explanation}",
                param.pos.getOrElse(Position.ofMacroExpansion)
              )
        }

        val default = Option.when(param.flags.is(Flags.HasDefault)) {
          val companion = sym.companionModule
          val method = companion.methodMember("$lessinit$greater$default$" + (i + 1)).head
          val call = Ref(companion).select(method)
          // the default of a generic class takes the class's type parameters
          if method.paramSymss.isEmpty then call else call.appliedToTypes(cls.typeArgs)
        }

        FieldInfo(param.name, fieldTpe, reader, default, param.annotations)

      // keys given literally must not clash; others depend on `fieldName`
      val literal = fields.flatMap(f => f.key.toList ++ f.deprecated)
      for (key, dups) <- literal.groupBy(identity) if dups.size > 1 do
        fail(s"since more than one of the fields of ${cls.show(using short)} has the key '$key'")
      fields

    def isCaseClass(sym: Symbol) = sym.isClassDef && sym.flags.is(Flags.Case)

    def assign(lhs: Expr[Any], rhs: Term): Expr[Unit] = Assign(lhs.asTerm, rhs).asExprOf[Unit]

    def warnUnknown(
        obj: Expr[Obj],
        known: Expr[String => Boolean],
        path: Expr[Vector[String]],
        reporter: Expr[Reporter]
    )(using Quotes): Expr[Unit] =
      '{
        for (key, v) <- $obj.fields if !$known(key) do
          v.unknown = true
          $reporter.warn(s"unknown key '${($path :+ key).mkString(".")}'", v.effectiveOrigin)
      }

    // The helpers below take their own `Quotes`, so that the expressions they
    // create belong to the splice they are called in.

    /** Read the field at key `name` into `x`, recording whether it failed. */
    def readField[T: Type](
        field: FieldInfo,
        name: Expr[String],
        x: Expr[T],
        obj: Expr[Obj],
        ok: Expr[Boolean],
        path: Expr[Vector[String]],
        reporter: Expr[Reporter]
    )(using Quotes): Expr[Unit] =
      def fail = assign(ok, Literal(BooleanConstant(false)))
      val secret = field.has[autoset.secret]
      val lookup: Expr[Option[(String, Value)]] =
        if field.deprecated.isEmpty then '{ $obj.fields.get($name).map(v => ($name, v)) }
        else
          '{
            ReaderUtils.lookupField(
              $obj,
              $name,
              ${ Expr(field.deprecated) },
              ${ Expr(secret) },
              $path,
              $reporter
            )
          }
      '{
        $lookup match
          case Some((key, v)) =>
            // before reading, so that errors don't show it
            ${ if secret then '{ v.markSecret() } else '{ () } }
            ${
              Select
                .unique(field.reader, "read")
                .appliedToArgs(List('v.asTerm, '{ $path :+ key }.asTerm, reporter.asTerm))
                .asExprOf[Option[T]]
            } match
              case Some(a) => ${ assign(x, 'a.asTerm) }
              case None => $fail
          case None =>
            ${
              field.default match
                case Some(default) => assign(x, default)
                case None if field.tpe.typeSymbol == TypeRepr.of[Option[Any]].typeSymbol =>
                  assign(x, '{ None }.asTerm)
                case None =>
                  '{
                    ReaderUtils.missingField($obj, $path :+ $name, $reporter)
                    $fail
                  }
            }
      }

    /** Read `obj` as the case class `T`.
      *
      * @param tag
      *   the key of the discriminator, if `T` is a case of a sealed type,
      *   which is not an unknown key
      */
    def readCaseClass[T: Type](
        obj: Expr[Obj],
        tag: Option[Expr[String]],
        path: Expr[Vector[String]],
        reporter: Expr[Reporter]
    )(using Quotes): Expr[Option[T]] =
      val cls = TypeRepr.of[T]
      val fields = fieldsOf(cls)

      /** Declare a variable for each field from the `i`th on and read it, then
        * construct the case class from the variables.
        *
        * @param vars
        *   the variables of the fields before the `i`th, in reverse
        * @param nameExprs
        *   the config names of the fields before the `i`th, in reverse
        */
      def readFields(
          i: Int,
          vars: List[Term],
          nameExprs: List[Expr[String]],
          ok: Expr[Boolean]
      )(using Quotes): Expr[Option[T]] =
        if i < fields.size then
          val field = fields(i)
          field.tpe.asType match
            case '[t] =>
              '{
                val name = ${ field.key.fold('{ $api.fieldName(${ Expr(field.name) }) })(Expr(_)) }
                var x: t = null.asInstanceOf[t]
                ${ readField[t](field, 'name, 'x, obj, ok, path, reporter) }
                ${ readFields(i + 1, 'x.asTerm :: vars, 'name :: nameExprs, ok) }
              }
        else
          val construct = New(Inferred(cls))
            .select(cls.typeSymbol.primaryConstructor)
            .appliedToTypes(cls.typeArgs)
            .appliedToArgs(vars.reverse)
            .asExprOf[T]
          '{
            val names = ${ Expr.ofList(nameExprs.reverse) } ++ ${ Expr(fields.flatMap(_.deprecated)) }
            ${
              val isTag: Expr[String => Boolean] = tag match
                case Some(t) => '{ (key: String) => key == $t }
                case None => '{ (_: String) => false }
              warnUnknown(obj, '{ key => names.contains(key) || $isTag(key) }, path, reporter)
            }
            if $ok then Some($construct) else None
          }

      '{
        var ok = true
        ${ readFields(0, Nil, Nil, 'ok) }
      }

    /** A case of a sealed type: a case class, or a singleton if `singleton`. */
    case class Case(name: String, sym: Symbol, singleton: Boolean)

    /** The cases of the sealed type `sym`, with nested sealed types flattened. */
    def casesOf(sym: Symbol): List[Case] =
      sym.children.flatMap { child =>
        if child.isTerm then List(Case(child.name, child, true)) // enum case without parameters
        else if child.flags.is(Flags.Module) then
          List(Case(child.companionModule.name, child.companionModule, true)) // case object
        else if isCaseClass(child) then List(Case(child.name, child, false))
        else if child.flags.is(Flags.Sealed) then casesOf(child)
        else fail(s"since its subtype ${child.name} is not a case class, case object or sealed")
      }

    def readSum(
        value: Expr[Value],
        path: Expr[Vector[String]],
        reporter: Expr[Reporter]
    )(using Quotes): Expr[Option[A]] =
      if tpe.typeArgs.nonEmpty then fail("since generic sealed types are not supported")
      val cases = casesOf(tpe.typeSymbol)
      if cases.isEmpty then fail("since it has no cases")
      for (name, dups) <- cases.groupBy(_.name) if dups.size > 1 do
        fail(s"since more than one of its cases is called $name")
      val allSingletons = cases.forall(_.singleton)

      /** Read `obj` as the case whose config name is `name`.
        *
        * @param tagValue
        *   the value of the discriminator, at `tagPath`
        */
      def dispatch(
          name: Expr[String],
          names: Expr[List[String]],
          expected: Expr[String],
          disc: Expr[String],
          tagValue: Expr[Value],
          tagPath: Expr[Vector[String]],
          obj: Expr[Obj]
      )(using Quotes): Expr[Option[A]] =
        cases.zipWithIndex.foldRight(
          '{ ReaderUtils.mismatch($expected, $tagValue, $tagPath, $reporter) }
        ) { case ((c, i), otherwise) =>
          val read: Expr[Option[A]] =
            if c.singleton then
              '{
                ${ warnUnknown(obj, '{ _ == $disc }, path, reporter) }
                Some(${ Ref(c.sym).asExprOf[A] })
              }
            else
              c.sym.typeRef.asType match
                case '[t] =>
                  readCaseClass[t](obj, Some(disc), path, reporter).asExprOf[Option[A]]
          '{ if $name == $names(${ Expr(i) }) then $read else $otherwise }
        }

      '{
        val disc = $api.discriminator
        val names = ${ Expr.ofList(cases.map(c => '{ $api.caseName(${ Expr(c.name) }) })) }
        val expected = names.map(n => s"'$n'").mkString("one of ", ", ", "")
        $value match
          case str @ Str(raw, _, origins) =>
            // short for an object with only the discriminator
            val obj = Obj(m.LinkedHashMap(disc -> str), origins)
            ${ dispatch('{ raw.trim }, 'names, 'expected, 'disc, 'str, path, 'obj) }
          case obj: Obj =>
            obj.fields.get(disc) match
              case Some(tag @ Str(raw, _, _)) =>
                ${ dispatch('{ raw.trim }, 'names, 'expected, 'disc, 'tag, '{ $path :+ disc }, 'obj) }
              case Some(other) => ReaderUtils.mismatch(expected, other, $path :+ disc, $reporter)
              case None => ReaderUtils.missingField(obj, $path :+ disc, $reporter)
          case _ =>
            ReaderUtils.mismatch(
              ${ if allSingletons then 'expected else '{ "an object" } },
              $value,
              $path,
              $reporter
            )
      }

    /** The strings of a union of string literal types, if `t` is one. */
    def literals(t: TypeRepr): Option[List[String]] = t.dealias match
      case ConstantType(StringConstant(s)) => Some(List(s))
      case OrType(l, r) => for ls <- literals(l); rs <- literals(r) yield ls ++ rs
      case _ => None

    def readLiterals(
        strings: List[String],
        value: Expr[Value],
        path: Expr[Vector[String]],
        reporter: Expr[Reporter]
    )(using Quotes): Expr[Option[A]] =
      val expected = strings.distinct.map(s => s"'$s'").mkString("one of ", ", ", "")
      '{
        $value match
          case Str(raw, _, _) if ${ Expr(strings) }.contains(raw.trim) =>
            Some(raw.trim.asInstanceOf[A])
          case _ => ReaderUtils.mismatch(${ Expr(expected) }, $value, $path, $reporter)
      }

    def body(
        value: Expr[Value],
        path: Expr[Vector[String]],
        reporter: Expr[Reporter]
    )(using Quotes): Expr[Option[A]] =
      val sym = tpe.typeSymbol
      literals(tpe) match
        case Some(strings) => readLiterals(strings, value, path, reporter)
        case None if isCaseClass(sym) =>
          '{
            $value match
              case obj: Obj => ${ readCaseClass[A]('obj, None, path, reporter) }
              case _ => ReaderUtils.mismatch("an object", $value, $path, $reporter)
          }
        case None if sym.flags.is(Flags.Sealed) => readSum(value, path, reporter)
        case None =>
          fail("which is not a case class, sealed type, enum or union of string literals")

    // `Reader` has a single abstract method, so the reader is a closure of
    // `read`, typed as `api.Reader[A]` (like `Lambda`, but for a SAM type)
    val readSym = Symbol.newMethod(
      Symbol.spliceOwner,
      "$anonfun",
      MethodType(List("value", "path", "reporter"))(
        _ => List(TypeRepr.of[Value], TypeRepr.of[Vector[String]], TypeRepr.of[Reporter]),
        _ => TypeRepr.of[Option[A]]
      )
    )
    val readDef = DefDef(
      readSym,
      {
        case List(List(value: Term, path: Term, reporter: Term)) =>
          val rhs = body(
            value.asExprOf[Value],
            path.asExprOf[Vector[String]],
            reporter.asExprOf[Reporter]
          )
          Some(rhs.asTerm.changeOwner(readSym))
        case _ => None
      }
    )
    Block(List(readDef), Closure(Ref(readSym), Some(readerOf(tpe)))).asExprOf[R]
