package test

import autoset.model.*
import autoset.secret
import utest.*

case class ReadDb(host: String, port: Int = 5432, @secret password: String) derives autoset.Reader
case class ReadApp(name: String, db: ReadDb, data: os.Path) derives autoset.Reader

object ReadTest extends TestSuite:

  /** Write `files` to a temporary directory and read `paths` from it as an `A`. */
  def read[A: autoset.Reader](
      files: Seq[(String, String)],
      paths: Seq[String],
      env: Map[String, String] = Map(),
      args: Seq[Arg] = Seq(),
      valueDirs: Seq[String] = Seq(),
      base: Option[A] = None
  ): (Option[(A, Obj)], String, os.Path) =
    val dir = os.temp.dir()
    for (name, content) <- files do os.write(dir / os.RelPath(name), content, createFolders = true)
    val reporter = Reporter()
    val result = autoset.read[A](
      paths.map(os.FilePath(_)),
      pwd = dir,
      valueDirs = valueDirs.map(os.FilePath(_)),
      env = env,
      envPrefix = "APP_",
      props = Map(),
      args = args,
      base = base,
      reporter = reporter
    )
    assert(result.isEmpty == reporter.hasErrors)
    (result, reporter.render, dir)

  val tests = Tests {
    test("read") {
      val (result, out, dir) = read[ReadApp](
        Seq(
          "conf/app.json" ->
            """{"name": "app", "data": "data", "db": {"host": "h", "password": "hunter2", "pasword": "oops"}}"""
        ),
        Seq("conf/app.json"),
        env = Map("APP_DB_PORT" -> "1234")
      )
      val (app, config) = result.get
      assert(app == ReadApp("app", ReadDb("h", 1234, "hunter2"), dir / "conf" / "data"))
      assert(out == "warning: conf/app.json:1:87: unknown key 'db.pasword'\n")

      // the merged config doesn't show the secret, nor the misspelled key
      assert(
        config.pretty() ==
          """{ // conf/app.json
            |  name: "app",
            |  data: "data",
            |  db: {
            |    host: "h",
            |    password: <secret>,
            |    pasword: <unknown>,
            |    port: "1234" // env APP_DB_PORT
            |  }
            |}""".stripMargin
      )
    }
    test("load errors") {
      // reading is not attempted if loading fails
      val (result, out, _) = read[ReadApp](Seq("app.json" -> "{"), Seq("app.json"))
      assert(result.isEmpty)
      assert(out.startsWith("error: app.json:"))
      assert(!out.contains("missing required field"))
    }
    test("read errors") {
      val (result, out, _) = read[ReadApp](
        Seq("app.json" -> """{"name": "app", "data": "d", "db": {"host": "h", "port": "x", "password": "p"}}"""),
        Seq("app.json")
      )
      assert(result.isEmpty)
      assert(out == "error: app.json:1:58: expected an integer for 'db.port', found 'x'\n")
    }
    test("args") {
      val files = Seq("app.json" -> """{"name": "app", "data": "d", "db": {"host": "h", "password": "p"}}""")
      val (result, _, _) = read[ReadApp](files, Seq("app.json"), args = Seq(Arg("--db-port", List("db", "port"), "80")))
      assert(result.get._1.db.port == 80)
      // errors point at the argument
      val (failed, out, _) = read[ReadApp](files, Seq("app.json"), args = Seq(Arg("--db-port", List("db", "port"), "http")))
      assert(failed.isEmpty)
      assert(out == "error: arg --db-port: expected an integer for 'db.port', found 'http'\n")
    }
    test("missing fields point at files") {
      // a value file or an environment variable contributes to the objects on
      // its path, but it holds a single value, so it is no place to add a
      // field: the error points at the file which declared the object
      val files = Seq(
        "app.json" -> """{"name": "app", "data": "d", "db": {"host": "h"}}""",
        "secrets/db.password" -> "hunter2\n"
      )
      def readApp[A: autoset.Reader] =
        read[A](files, Seq("app.json"), env = Map("APP_DB_PORT" -> "1234"), valueDirs = Seq("secrets"))

      val (result, out, _) = readApp[ReadApp]
      assert(result.get._1.db == ReadDb("h", 1234, "hunter2"))
      assert(out == "")

      // the value file and the environment variable are the most recent
      // contributors to the root object and to `db`
      val config = result.get._2
      assert(config.effectiveOrigin == Origin.Env("APP_DB_PORT"))
      // but only the file declared them, and that is where a field would go
      assert(config.declarationOrigin.map(_.pretty) == Some("app.json:1:1"))
      val db = config.fields("db").asInstanceOf[Obj]
      assert(db.effectiveOrigin == Origin.Env("APP_DB_PORT"))
      assert(db.declarationOrigin.map(_.pretty) == Some("app.json:1:36"))

      case class Strict(name: String, db: ReadDb, data: os.Path, nope: String) derives autoset.Reader
      val (failed, errors, _) = readApp[Strict]
      assert(failed.isEmpty)
      assert(errors == "error: app.json:1:1: missing required field 'nope'\n")
    }
    test("missing fields with nothing to point at") {
      // no file declares an object here, so there is no place to add the field,
      // and the error has no origin rather than a misleading one
      val (result, out, _) = read[ReadApp](
        Seq("secrets/db.password" -> "hunter2"),
        Seq(),
        env = Map("APP_NAME" -> "app"),
        valueDirs = Seq("secrets")
      )
      assert(result.isEmpty)
      assert(
        out ==
          """error: missing required field 'db.host'
            |error: missing required field 'data'
            |""".stripMargin
      )
    }
    test("the config shows the defaults that were used") {
      val (result, out, dir) = read[ReadApp](
        Seq("app.json" -> """{"name": "app", "data": "d", "db": {"host": "h", "password": "p"}}"""),
        Seq("app.json")
      )
      val (app, config) = result.get
      assert(app.db.port == 5432)
      assert(out == "")
      // `db.port` was not set anywhere, so the reader recorded what it used
      assert(
        config.pretty() ==
          """{ // app.json
            |  name: "app",
            |  data: "d",
            |  db: {
            |    host: "h",
            |    password: <secret>,
            |    port: "5432" // default
            |  }
            |}""".stripMargin
      )
      // and reading the same config again gives the same result, rather than
      // parsing the recorded value back
      val reporter = Reporter()
      assert(autoset.project[ReadApp](config, reporter) == Some(app))
      assert(reporter.render == "")
    }
    test("a base gives defaults at run time") {
      // defaults which are only known at run time, overriding the field's own
      val files = Seq("app.json" -> """{"name": "app", "data": "d", "db": {"host": "h"}}""")
      val base = ReadApp("base", ReadDb("base-host", 1234, "hunter2"), os.pwd)
      val (result, out, dir) = read[ReadApp](files, Seq("app.json"), base = Some(base))
      val (app, config) = result.get
      assert(app == ReadApp("app", ReadDb("h", 1234, "hunter2"), dir / "d"))
      assert(out == "")
      assert(config.fields("db").asInstanceOf[Obj].fields("port").origins == List(Origin.Default))
    }
    test("secret errors") {
      case class Port(@secret port: Int) derives autoset.Reader
      val (result, out, _) = read[Port](Seq("app.json" -> """{"port": "hunter2"}"""), Seq("app.json"))
      assert(result.isEmpty)
      assert(out == "error: app.json:1:10: expected an integer for 'port', found <secret>\n")
    }
  }
