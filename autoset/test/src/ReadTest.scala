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
      env: Map[String, String] = Map()
  ): (Option[(A, Obj)], String, os.Path) =
    val dir = os.temp.dir()
    for (name, content) <- files do os.write(dir / os.RelPath(name), content, createFolders = true)
    val reporter = Reporter()
    val result = autoset.read[A](
      paths.map(os.FilePath(_)),
      pwd = dir,
      env = env,
      envPrefix = "APP_",
      props = Map(),
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
    test("secret errors") {
      case class Port(@secret port: Int) derives autoset.Reader
      val (result, out, _) = read[Port](Seq("app.json" -> """{"port": "hunter2"}"""), Seq("app.json"))
      assert(result.isEmpty)
      assert(out == "error: app.json:1:10: expected an integer for 'port', found <secret>\n")
    }
  }
