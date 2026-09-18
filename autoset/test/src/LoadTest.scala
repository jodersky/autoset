package test

import autoset.model.*
import utest.*

object LoadTest extends TestSuite:

  /** Write `files` to a temporary directory and load `paths` from it. */
  def load(
      files: Seq[(String, String)],
      paths: Seq[String],
      env: Map[String, String] = Map(),
      envPrefix: String = null,
      envKeyReplacer: String => List[String] = autoset.defaultEnvKeyReplacer,
      envBinds: Seq[(String, List[String])] = Seq(),
      props: Map[String, String] = Map(),
      propsPrefix: String = null,
      propsBinds: Seq[(String, List[String])] = Seq(),
      args: Seq[Arg] = Seq(),
      parsers: Map[String, FormatParser] = autoset.defaultParsers,
      reporter: Reporter = null
  ): (Option[Obj], String) =
    val dir = os.temp.dir()
    for (name, content) <- files do os.write(dir / os.RelPath(name), content, createFolders = true)
    val out = java.io.ByteArrayOutputStream()
    val result = autoset.load(
      paths.map(os.FilePath(_)),
      pwd = dir,
      parsers = parsers,
      env = env,
      envPrefix = envPrefix,
      envKeyReplacer = envKeyReplacer,
      envBinds = envBinds,
      props = props,
      propsPrefix = propsPrefix,
      propsBinds = propsBinds,
      args = args,
      reporter = if reporter != null then reporter else Reporter.printing(java.io.PrintStream(out))
    )
    (result, out.toString)

  def check(result: (Option[Obj], String), expected: String, reported: String = "") =
    val (obj, out) = result
    assert(out == reported.stripMargin)
    val actual = obj.get.pretty()
    assert(actual == expected.stripMargin.trim)

  val tests = Tests {
    test("precedence") {
      val files = Seq(
        "dir/1.json" -> """|{
                           |  "key1": "1",
                           |  "mkey1": {"a": {"a": "1", "b": "1"}},
                           |  "arr": [1, 2]
                           |}""".stripMargin,
        "dir/2.json" -> """|{
                           |  "key2": "2",
                           |  "mkey1": {
                           |    "a": {"b": "2", "c": "2"},
                           |    "b": {"a": {"a": "2"}}
                           |  }
                           |}""".stripMargin,
        "file.json" -> """|{
                          |  "key_file": "file",
                          |  "mkey1": {"b": "file", "c": null},
                          |  "arr": [3],
                          |  "props": {"a": "file", "b": "file", "c": "file", "d": "file"},
                          |  "env": {"a": "file", "b": "file", "c": "file", "d": "file"}
                          |}""".stripMargin
      )
      check(
        load(
          files,
          Seq("dir", "file.json"),
          env = Map(
            "UNUSED" -> "ok",
            "APP_ENV_A" -> "env",
            "APP_ENV_B" -> "env",
            "EXTRA_COOL" -> "env"
          ),
          envPrefix = "APP_",
          envBinds = Seq("EXTRA_COOL" -> List("env", "c"), "UNSET" -> List("env", "e")),
          props = Map(
            "unused" -> "ok",
            "app.props.a" -> "props",
            "app.props.b" -> "props",
            "extra.cool" -> "props"
          ),
          propsPrefix = "app.",
          propsBinds = Seq("extra.cool" -> List("props", "c"), "unset" -> List("props", "e"))
        ),
        """|{ // file.json
           |  key1: "1", // dir/1.json:2:11
           |  mkey1: {
           |    a: { // dir/2.json
           |      a: "1", // dir/1.json:3:24
           |      b: "2", // dir/2.json:4:16 (overrides dir/1.json:3:34)
           |      c: "2"
           |    },
           |    b: "file", // file.json:3:18 (overrides dir/2.json:5:10)
           |    c: null
           |  },
           |  arr: ["3"], // file.json:4:10 (overrides dir/1.json:4:10)
           |  key2: "2", // dir/2.json:2:11
           |  key_file: "file",
           |  props: {
           |    a: "props", // prop app.props.a (overrides file.json:5:18)
           |    b: "props", // prop app.props.b (overrides file.json:5:31)
           |    c: "props", // prop extra.cool (overrides file.json:5:44)
           |    d: "file"
           |  },
           |  env: {
           |    a: "env", // env APP_ENV_A (overrides file.json:6:16)
           |    b: "env", // env APP_ENV_B (overrides file.json:6:29)
           |    c: "env", // env EXTRA_COOL (overrides file.json:6:42)
           |    d: "file"
           |  }
           |}""",
        """|warning: file.json:3:18: 'mkey1.b' is set to a value, replacing an object from dir/2.json:5:10
           |"""
      )
    }
    test("directories") {
      // sorted, dotfiles skipped, subdirectories skipped with a warning
      check(
        load(
          Seq(
            "dir/b.json" -> """{"a": "b"}""",
            "dir/a.json" -> """{"a": "a", "x": "a"}""",
            "dir/.hidden.json" -> """{"a": "hidden"}""",
            "dir/sub/c.json" -> """{"a": "c"}"""
          ),
          Seq("dir")
        ),
        """|{ // dir/b.json
           |  a: "b", // dir/b.json:1:7 (overrides dir/a.json:1:7)
           |  x: "a" // dir/a.json:1:17
           |}""",
        """|warning: skipping dir/sub in config directory dir, which is not a regular file
           |"""
      )
    }
    test("formats") {
      check(
        load(
          Seq(
            "a.json" -> """{"json": 1}""",
            "b.yaml" -> "yaml: 1",
            "c.yml" -> "yml: 1",
            "d.conf" -> "hocon = 1",
            "e.properties" -> "props=1",
            "f.ini" -> "ini = 1",
            "g" -> "noext = 1"
          ),
          Seq("a.json", "b.yaml", "c.yml", "d.conf", "e.properties", "f.ini", "g")
        ),
        """|{ // a.json
           |  json: "1",
           |  yaml: "1", // b.yaml:1:7
           |  yml: "1", // c.yml:1:6
           |  hocon: "1", // d.conf:1
           |  props: "1", // e.properties
           |  ini: "1", // f.ini:1:7
           |  noext: "1" // g:1:9
           |}"""
      )
    }
    test("custom parsers") {
      val parsers = autoset.defaultParsers + ("txt" -> autoset.parsers.JsonParser)
      check(
        load(Seq("a.txt" -> """{"a": 1}"""), Seq("a.txt"), parsers = parsers),
        """|{ // a.txt
           |  a: "1"
           |}"""
      )
    }
    test("errors") {
      test("unknown extension") {
        val (result, out) = load(Seq("a.toml" -> "a = 1"), Seq("a.toml"))
        assert(result.isEmpty)
        assert(out == "error: no parser defined for extension of file a.toml\n")
      }
      test("missing path") {
        val (result, out) = load(Seq(), Seq("nope.json"))
        assert(result.isEmpty)
        assert(out == "error: path nope.json does not exist\n")
      }
      test("all files are parsed") {
        val (result, out) = load(
          Seq("a.json" -> "{", "b.json" -> """{"a": 1}""", "c.yaml" -> "a: \"x"),
          Seq("a.json", "b.json", "c.yaml")
        )
        assert(result.isEmpty)
        assert(
          out ==
            """|error: a.json:1:2: unexpected end of file
               |{
               | ^
               |error: c.yaml:1:4: Expected closing " but reached EOF
               |a: "x
               |   ^
               |""".stripMargin
        )
      }
      test("earlier errors of a shared reporter") {
        val out = java.io.ByteArrayOutputStream()
        val reporter = Reporter.printing(java.io.PrintStream(out))
        reporter.error("unrelated")
        val (result, _) = load(Seq("a.json" -> """{"a": 1}"""), Seq("a.json"), reporter = reporter)
        assert(result.isDefined)
      }
    }
    test("env") {
      test("nested keys") {
        check(
          load(
            Seq(),
            Seq(),
            env = Map("APP_DB_PORT" -> "1", "APP_DB_HOST" -> "h", "APP_X" -> "x", "OTHER" -> "o"),
            envPrefix = "APP_"
          ),
          """|{
             |  db: {
             |    host: "h", // env APP_DB_HOST
             |    port: "1" // env APP_DB_PORT
             |  },
             |  x: "x" // env APP_X
             |}"""
        )
      }
      test("custom key replacer") {
        check(
          load(
            Seq(),
            Seq(),
            env = Map("APP_DB__MAX_SIZE" -> "1"),
            envPrefix = "APP_",
            envKeyReplacer = _.toLowerCase.split("__").toList
          ),
          """|{
             |  db: {
             |    max_size: "1" // env APP_DB__MAX_SIZE
             |  }
             |}"""
        )
      }
      test("invalid keys") {
        check(
          load(Seq(), Seq(), env = Map("APP_A__B" -> "1", "APP_" -> "2"), envPrefix = "APP_"),
          "{} // default",
          """|warning: env APP_: ignoring invalid configuration key ''
             |warning: env APP_A__B: ignoring invalid configuration key 'a..b'
             |"""
        )
      }
      test("binds") {
        check(
          load(
            Seq(),
            Seq(),
            env = Map("PORT" -> "80"),
            envBinds = Seq("PORT" -> List("http", "port"), "UNSET" -> List("x"))
          ),
          """|{
             |  http: {
             |    port: "80" // env PORT
             |  }
             |}"""
        )
      }
    }
    test("args") {
      check(
        load(
          Seq("a.yaml" -> "port: 1\ndb:\n  host: h\n"),
          Seq("a.yaml"),
          env = Map("APP_PORT" -> "2"),
          envPrefix = "APP_",
          props = Map("app.port" -> "3"),
          propsPrefix = "app.",
          args = Seq(
            Arg("--port", List("port"), "4"),
            Arg("-p", List("port"), "5"),
            Arg("--db", List("db"), "postgres://h"),
            Arg("--set log.level=debug", List("log", "level"), "debug")
          )
        ),
        """|{
           |  port: "5", // arg -p (overrides arg --port, prop app.port, env APP_PORT, a.yaml:1:7)
           |  db: "postgres://h", // arg --db (overrides a.yaml:3:3)
           |  log: {
           |    level: "debug" // arg --set log.level=debug
           |  }
           |}""",
        """|warning: arg --db: 'db' is set to a value, replacing an object from a.yaml:3:3
           |"""
      )
    }
    test("type conflicts") {
      check(
        load(
          Seq("a.yaml" -> "db:\n  host: h\nname: n\n"),
          Seq("a.yaml"),
          env = Map("APP_DB" -> "postgres://h"),
          envPrefix = "APP_",
          props = Map("app.name.first" -> "f"),
          propsPrefix = "app."
        ),
        """|{
           |  db: "postgres://h", // env APP_DB (overrides a.yaml:2:3)
           |  name: {
           |    first: "f" // prop app.name.first
           |  }
           |}""",
        """|warning: env APP_DB: 'db' is set to a value, replacing an object from a.yaml:2:3
           |warning: prop app.name.first: 'name' is set to an object, replacing a value from a.yaml:3:7
           |"""
      )
    }
    test("init") {
      val init = Obj(collection.mutable.LinkedHashMap(), List(Origin.Default))
      init.fields("a") = Str("1", LitKind.String, List(Origin.Default))
      val out = java.io.ByteArrayOutputStream()
      val result = autoset.load(
        env = Map("APP_B" -> "2"),
        envPrefix = "APP_",
        props = Map(),
        init = init,
        reporter = Reporter.printing(java.io.PrintStream(out))
      )
      assert(result.get eq init)
      assert(init.fields.keys.toList == List("a", "b"))
    }
    test("relative paths with a custom pwd") {
      // `pwd` differs from the process's working directory, so paths from
      // files must be resolved with the files' absolute paths
      val dir = os.temp.dir()
      os.write(dir / "conf" / "app.json", """{"data": "db", "nested": {"logs": ["../logs"]}}""", createFolders = true)
      val out = java.io.ByteArrayOutputStream()
      val result = autoset.load(
        Seq(os.FilePath("conf")),
        pwd = dir,
        env = Map("APP_CACHE" -> "cache"),
        envPrefix = "APP_",
        props = Map(),
        reporter = Reporter.printing(java.io.PrintStream(out))
      ).get

      def resolve(value: Value) =
        autoset.default.OsPathReader.read(value, Vector.empty, Reporter()).get
      assert(resolve(result.fields("data")) == dir / "conf" / "db")
      val logs = result.fields("nested").asInstanceOf[Obj].fields("logs").asInstanceOf[Arr].values.head
      assert(resolve(logs) == dir / "logs")
      // values from the environment are still relative to the working directory
      assert(resolve(result.fields("cache")) == os.pwd / "cache")

      // file names are still shown relative to `pwd`
      assert(result.fields("data").origins == List(
        Origin.File("conf/app.json", 9, 1, 10, Some((dir / "conf" / "app.json").toString))
      ))
      assert(result.fields("data").effectiveOrigin.pretty == "conf/app.json:1:10")
    }
  }
