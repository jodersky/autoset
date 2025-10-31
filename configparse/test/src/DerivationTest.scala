package test

import utest._

import configparse.derivation.Result.Success
import configparse.derivation.Result.Error
import configparse.derivation.FieldError
import configparse.model.ReadException

object DerivationTest extends TestSuite {
  def tests: Tests = Tests{
    test("basic") {
      case class Inner(bar: Int = 42) derives configparse.Reader
      case class MyConfig(fooBar: Int = 2, inner: Inner) derives configparse.Reader

      test("defaults") {
        configparse.read[MyConfig]() ==> MyConfig(2, Inner(42))
      }
      test("override") {
        val cfg = configparse.readConfig(args = Map("foo_bar" -> "1", "inner.bar" -> "2"))
        configparse.read[MyConfig](config = cfg) ==> MyConfig(1, Inner(2))
      }
      test("override2") {
        configparse.read[MyConfig](
          args = Map("foo_bar" -> "1", "inner.bar" -> "2")
        ) ==> MyConfig(1, Inner(2))
      }
    }
    // test("missing") {
    //   case class Inner(bar: Int) derives configparse.default.Reader
    //   case class MyConfig(fooBar: Int, inner: Inner) derives configparse.default.Reader

    //   test("defaults") {
    //     val errs = configparse.default.readResult[MyConfig](configparse.read()).asInstanceOf[Error].errors
    //     errs ==> Seq(
    //       FieldError.TypeMismatch("foo_bar", configparse.Null(), "int"),
    //       FieldError.TypeMismatch("inner.bar", configparse.Null(), "int")
    //     )
    //   }
    //   test("partial1") {
    //     val errs = configparse.default.readResult[MyConfig](configparse.read(args=Map("foo_bar" -> "1"))).asInstanceOf[Error].errors
    //     errs ==> Seq(
    //       FieldError.TypeMismatch("inner.bar", configparse.Null(), "int")
    //     )
    //   }
    //   test("partial2") {
    //     val errs = configparse.default.readResult[MyConfig](configparse.read(args=Map("inner.bar" -> "0"))).asInstanceOf[Error].errors
    //     errs ==> Seq(
    //       FieldError.TypeMismatch("foo_bar", configparse.Null(), "int")
    //     )
    //   }
    //   test("mix") {
    //     val errs = configparse.default.readResult[MyConfig](configparse.read(args=Map("inner.bar" -> "a"))).asInstanceOf[Error].errors
    //     errs ==> Seq(
    //       FieldError.TypeMismatch("foo_bar", configparse.Null(), "int"),
    //       FieldError.TypeMismatch("inner.bar", configparse.Str("a"), "int")
    //     )
    //   }
    //   test("ok") {
    //     configparse.default.readResult[MyConfig](configparse.read(args=Map("inner.bar" -> "1", "foo_bar" -> "2"))) ==>
    //       Success(
    //         MyConfig(2, Inner(1))
    //       )
    //   }
    // }
    // test("paths"){
    //   case class MyConfig(path: os.Path) derives configparse.default.Reader

    //   configparse.default.readResult[MyConfig](configparse.read(args=Map("path" -> "foo/bar"))) ==> Success(
    //     MyConfig(os.pwd / "foo" / "bar")
    //   )

    //   val cfg1 = configparse.read(args=Map("path" -> "foo/bar"))

    //   // pretend that the config was actually read from a file
    //   // TODO: actually read from a file and avoid this hack
    //   cfg1.fields.foreach(_._2.origins = List(configparse.Origin.File("/config", 1, 1)))

    //   configparse.default.readResult[MyConfig](cfg1) ==> Success(
    //     MyConfig(os.root / "config" / "foo" / "bar")
    //   )
    // }
  }

}
