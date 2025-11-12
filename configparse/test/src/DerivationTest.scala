package test

import utest._

import configparse.derivation.Result.Success
import configparse.derivation.Result.Error
import configparse.model.ReadException
import configparse.derivation.Invalid
import configparse.model.Path

object DerivationTest extends TestSuite {
  def tests: Tests = Tests{
    test("basic") {
      case class Inner(bar: Int = 42) derives configparse.Reader
      case class MyConfig(fooBar: Int = 2, inner: Inner = Inner()) derives configparse.Reader

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
    test("missing") {
      case class Inner(bar: Int) derives configparse.default.Reader
      case class MyConfig(fooBar: Int, inner: Inner) derives configparse.default.Reader

      test("defaults") {
        val errs = configparse.readResult[MyConfig]().asInstanceOf[Error].missing

        errs ==> Seq(
          Path("foo_bar"),
          Path("inner")
        )
      }
      test("partial1") {
        val errs = configparse.readResult[MyConfig](args=Map("foo_bar" -> "1")).asInstanceOf[Error].missing
        errs ==> Seq(
          Path("inner")
        )
      }
      test("partial2") {
        val errs = configparse.readResult[MyConfig](args=Map("inner.bar" -> "0")).asInstanceOf[Error].missing
        errs ==> Seq(
          Path("foo_bar")
        )
      }
      test("mix") {
        val err = configparse.readResult[MyConfig](args=Map("inner.bar" -> "a")).asInstanceOf[Error]
        err.missing ==> Seq(
          Path("foo_bar")
        )
        err.invalid ==> Seq(
          Invalid(
            Path("inner", "bar"),
            configparse.Str("a"),
            "'a' is not an integral number"
          )
        )
      }
      test("ok") {
        configparse.readResult[MyConfig](args=Map("inner.bar" -> "1", "foo_bar" -> "2")) ==>
          Success(
            MyConfig(2, Inner(1))
          )
      }
      test("config"){
        val cfg = configparse.readConfig(args=Map("inner.bar" -> "1", "foo_bar" -> "2"))
        configparse.readResult[MyConfig](config=cfg) ==>
          Success(
            MyConfig(2, Inner(1))
          )
      }
    }
    test("paths"){
      case class MyConfig(path: os.Path) derives configparse.Reader

      configparse.readResult[MyConfig](args=Map("path" -> "foo/bar")) ==> Success(
        MyConfig(os.pwd / "foo" / "bar")
      )

      val cfg1 = configparse.readConfig(args=Map("path" -> "foo/bar"))

      // pretend that the config was actually read from a file
      // TODO: actually read from a file and avoid this hack
      cfg1.fields.foreach(_._2.origins = List(configparse.Origin.File("/config/dummy.yaml", 1, 1)))

      configparse.readResult[MyConfig](config = cfg1) ==> Success(
        MyConfig(os.root / "config" / "foo" / "bar")
      )
    }
  }

}
