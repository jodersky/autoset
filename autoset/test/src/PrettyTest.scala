package test

import autoset.model.*
import utest.*
import Helpers.*

object PrettyTest extends TestSuite:

  def check(v: Value, expected: String, verbose: Boolean = false) =
    val actual = v.pretty(verbose)
    assert(actual == expected.stripMargin.trim)

  /** A base file with values overridden from env and another file. */
  def merged(): Obj =
    val base = obj(file(1))(
      "port" -> str("8080", file(1)),
      "db" -> obj(file(2))("host" -> str("local", file(3))),
      "tags" -> arr(file(5))(str("a", file(5)), nul(file(5))),
      "empty" -> obj(file(6))()
    )
    merge(base, 
      obj(env(""))(
        "port" -> str("9090", env("PORT")),
        "db" -> obj(env("DB_PASSWORD"))("password" -> str("hunter2", env("DB_PASSWORD")))
      )
    )
    merge(base, 
      obj(file(1, "other.conf"))(
        "extra" -> obj(file(1, "other.conf"))(
          "x" -> str("1", file(1, "other.conf")),
          "y" -> str("2", file(2, "other.conf"))
        )
      )
    )
    base

  val tests = Tests {
    test("compact") {
      check(
        merged(),
        """|{ // app.conf
           |  port: "9090", // env PORT (overrides app.conf:1:1)
           |  db: {
           |    host: "local",
           |    password: "hunter2" // env DB_PASSWORD
           |  },
           |  tags: ["a", null],
           |  empty: {},
           |  extra: { // other.conf
           |    x: "1",
           |    y: "2"
           |  }
           |}"""
      )
    }
    test("verbose") {
      check(
        merged(),
        """|{ // other.conf:1:1, env , app.conf:1:1
           |  port: "9090", // env PORT (overrides app.conf:1:1)
           |  db: { // env DB_PASSWORD, app.conf:2:1
           |    host: "local", // app.conf:3:1
           |    password: "hunter2" // env DB_PASSWORD
           |  },
           |  tags: [ // app.conf:5:1
           |    "a", // app.conf:5:1
           |    null // app.conf:5:1
           |  ],
           |  empty: {}, // app.conf:6:1
           |  extra: { // other.conf:1:1
           |    x: "1", // other.conf:1:1
           |    y: "2" // other.conf:2:1
           |  }
           |}""",
        verbose = true
      )
    }
    test("leaf") {
      check(str("a", file(1)), """"a" // app.conf:1:1""")
      check(nul(env("X")), """null // env X""")
      check(str("a", env("A"), file(3)), """"a" // env A (overrides app.conf:3:1)""")
    }
    test("quoting") {
      check(
        obj(file(1))(
          "plain_key-1" -> str("say \"hi\"\n\t\\", file(1)),
          "dotted.key" -> str("", file(1)),
          "" -> str("x", file(1))
        ),
        """|{ // app.conf
           |  plain_key-1: "say \"hi\"\n\t\\",
           |  "dotted.key": "",
           |  "": "x"
           |}"""
      )
    }
    test("literals always quoted") {
      val v = obj(file(1))(
        "n" -> Str("1", LitKind.Num, List(file(1))),
        "b" -> Str("true", LitKind.Bool, List(file(1)))
      )
      check(
        v,
        """|{ // app.conf
           |  n: "1",
           |  b: "true"
           |}"""
      )
    }
    test("named sources always annotated") {
      // env values are never summarized, so the object gets no label
      check(
        obj(env(""))("a" -> str("1", env("A")), "b" -> str("2", env("B"))),
        """|{
           |  a: "1", // env A
           |  b: "2" // env B
           |}"""
      )
    }
    test("majority source labels container") {
      check(
        obj(file(1))(
          "a" -> str("1", file(1, "x.conf")),
          "b" -> str("2", file(2)),
          "c" -> str("3", file(3)),
          "d" -> str("4", file(4))
        ),
        """|{ // app.conf
           |  a: "1", // x.conf:1:1
           |  b: "2",
           |  c: "3",
           |  d: "4"
           |}"""
      )
    }
    test("ties go to first source") {
      check(
        obj(file(1))(
          "a" -> str("1", file(1, "x.conf")),
          "b" -> str("2", file(2)),
          "c" -> str("3", file(3)),
          "d" -> str("4", file(3, "x.conf"))
        ),
        """|{ // x.conf
           |  a: "1",
           |  b: "2", // app.conf:2:1
           |  c: "3", // app.conf:3:1
           |  d: "4"
           |}"""
      )
    }
    test("override within same source") {
      check(
        obj(file(1))("a" -> str("2", file(2), file(1))),
        """|{ // app.conf
           |  a: "2" // app.conf:2:1 (overrides app.conf:1:1)
           |}"""
      )
    }
    test("defaults") {
      // a default a reader filled in is the exception worth pointing at, so it
      // is annotated even where it is the enclosing object's only other source
      check(
        obj(Origin.Default)("a" -> str("1", Origin.Default), "b" -> str("2", file(1))),
        """|{ // app.conf
           |  a: "1", // default
           |  b: "2"
           |}"""
      )
      // unless everything in it is a default, and the label says it once
      check(
        obj(Origin.Default)("a" -> str("1", Origin.Default), "b" -> str("2", Origin.Default)),
        """|{ // default
           |  a: "1",
           |  b: "2"
           |}"""
      )
    }
    test("arrays") {
      test("short on one line") {
        check(
          obj(file(1))("a" -> arr(file(1))(str("1", file(1)), str("2", file(1)))),
          """|{ // app.conf
             |  a: ["1", "2"]
             |}"""
        )
      }
      test("long on several lines") {
        val items = (1 to 20).map(i => str(i.toString * 3, file(1)))
        val lines = items.indices.map(i => s"""    "${(i + 1).toString * 3}"""").mkString(",\n")
        check(
          obj(file(1))("a" -> arr(file(1))(items*)),
          s"""|{ // app.conf
              |  a: [
              |$lines
              |  ]
              |}"""
        )
      }
      test("annotated elements on several lines") {
        check(
          obj(file(1))("a" -> arr(file(1))(str("1", file(1)), str("2", env("X")))),
          """|{ // app.conf
             |  a: [
             |    "1",
             |    "2" // env X
             |  ]
             |}"""
        )
      }
      test("overridden") {
        check(
          obj(file(1))(
            "x" -> str("x", file(1)),
            "a" -> arr(file(4, "b.conf"), file(2))(str("1", file(4, "b.conf"))),
            "y" -> str("y", file(3))
          ),
          """|{ // app.conf
             |  x: "x",
             |  a: ["1"], // b.conf:4:1 (overrides app.conf:2:1)
             |  y: "y"
             |}"""
        )
      }
      test("nested containers") {
        check(
          obj(file(1))("a" -> arr(file(1))(obj(file(1))("b" -> str("1", file(1))))),
          """|{ // app.conf
             |  a: [
             |    {
             |      b: "1"
             |    }
             |  ]
             |}"""
        )
      }
    }
    test("redacted") {
      val v = merged()
      v.fields("db").asInstanceOf[Obj].fields("password").secret = true
      v.fields("extra").unknown = true
      v.fields("tags").asInstanceOf[Arr].values(0).secret = true
      check(
        v,
        """|{ // app.conf
           |  port: "9090", // env PORT (overrides app.conf:1:1)
           |  db: {
           |    host: "local",
           |    password: <secret> // env DB_PASSWORD
           |  },
           |  tags: [<secret>, null],
           |  empty: {},
           |  extra: <unknown> // other.conf:1:1
           |}"""
      )
      check(
        v,
        """|{ // other.conf:1:1, env , app.conf:1:1
           |  port: "9090", // env PORT (overrides app.conf:1:1)
           |  db: { // env DB_PASSWORD, app.conf:2:1
           |    host: "local", // app.conf:3:1
           |    password: <secret> // env DB_PASSWORD
           |  },
           |  tags: [ // app.conf:5:1
           |    <secret>, // app.conf:5:1
           |    null // app.conf:5:1
           |  ],
           |  empty: {}, // app.conf:6:1
           |  extra: <unknown> // other.conf:1:1
           |}""",
        verbose = true
      )
      assert(v.toString == v.pretty())
    }
    test("secret object") {
      // the whole object is hidden
      val v = obj(file(1))("db" -> obj(file(2))("a" -> str("1", file(2))))
      v.fields("db").markSecret()
      check(v, """|{ // app.conf
                  |  db: <secret>
                  |}""")
      assert(v.fields("db").asInstanceOf[Obj].fields("a").secret)
    }
    test("empty") {
      check(obj(file(1))(), "{} // app.conf:1:1")
      check(arr(env("A"))(), "[] // env A")
    }
  }
