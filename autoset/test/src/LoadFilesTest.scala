package test

import autoset.model.*
import utest.*

/** Loads configuration from the files in `resources/load/<dir>`, and compares
  * everything reported, followed by the resulting configuration, with
  * `resources/load/<name>.out`.
  *
  * Run with `OVERWRITE=yes` to update the expected output.
  */
object LoadFilesTest extends TestSuite:

  def check(
      name: String,
      paths: Seq[String],
      dir: String = null,
      env: Map[String, String] = Map(),
      envPrefix: String = null,
      envBinds: Seq[(String, List[String])] = Seq(),
      props: Map[String, String] = Map(),
      propsPrefix: String = null,
      propsBinds: Seq[(String, List[String])] = Seq()
  ): Unit =
    val root = Util.pwd / "load"
    val out = java.io.ByteArrayOutputStream()
    val result = autoset.load(
      paths.map(os.FilePath(_)),
      pwd = root / (if dir == null then name else dir),
      env = env,
      envPrefix = envPrefix,
      envBinds = envBinds,
      props = props,
      propsPrefix = propsPrefix,
      propsBinds = propsBinds,
      reporter = Reporter.printing(java.io.PrintStream(out))
    )
    val config = result.fold("(failed)")(_.pretty())
    DiffTools.assertNoDiff(root / s"$name.out", s"${out}---\n$config\n")

  val tests = Tests {
    test("precedence") {
      // directory entries in order, then files, env and props
      check(
        "precedence",
        Seq("conf.d", "app.json"),
        env = Map(
          "APP_SERVER_HOST" -> "0.0.0.0",
          "APP_DB_POOL_MIN" -> "5",
          "DATABASE_URL" -> "postgres://db",
          "UNRELATED" -> "x"
        ),
        envPrefix = "APP_",
        envBinds = Seq("DATABASE_URL" -> List("db", "url")),
        props = Map(
          "app.logging.level" -> "warn",
          "custom.port" -> "1234",
          "unrelated" -> "x"
        ),
        propsPrefix = "app.",
        propsBinds = Seq("custom.port" -> List("server", "port"))
      )
    }
    test("directories") {
      // sorted, dotfiles and subdirectories skipped, no extension is INI
      check("directories", Seq("dir", "last.yaml"))
    }
    test("env") {
      check(
        "env",
        Seq("base.yaml"),
        env = Map(
          "APP_DB_HOST" -> "db.example.com",
          "APP_DB_USER" -> "admin",
          "APP_NAME_FIRST" -> "value replaced by an object",
          "APP_TAGS" -> "c,d",
          "APP_A__B" -> "empty segment",
          "APP_" -> "empty key",
          "PORT" -> "6543",
          "APP_DB_PORT" -> "overridden by bind",
          "OTHER" -> "not read"
        ),
        envPrefix = "APP_",
        envBinds = Seq("PORT" -> List("db", "port"), "UNSET" -> List("x"))
      )
    }
    test("props") {
      check(
        "props",
        Seq("base.properties"),
        props = Map(
          "app.db.host" -> "db.example.com",
          "app.db.user" -> "admin",
          "app.name.first" -> "value replaced by an object",
          "app.a..b" -> "empty segment",
          "app." -> "empty key",
          "port" -> "6543",
          "app.db.port" -> "overridden by bind",
          "other" -> "not read"
        ),
        propsPrefix = "app.",
        propsBinds = Seq("port" -> List("db", "port"), "unset" -> List("x"))
      )
    }
    test("env and props") {
      // props take precedence over env
      check(
        "env-and-props",
        Seq("base.yaml"),
        dir = "env",
        env = Map("APP_DB_HOST" -> "env", "APP_NAME" -> "env"),
        envPrefix = "APP_",
        props = Map("app.db.host" -> "props"),
        propsPrefix = "app."
      )
    }
    test("conflicts") {
      check("conflicts", Seq("1.json", "2.yaml"))
    }
    test("errors") {
      // all files are parsed, to report all errors at once
      check("errors", Seq("."))
    }
    test("missing") {
      check("missing", Seq("present.json", "absent.json", "absent-dir"))
    }
  }
