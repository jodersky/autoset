package test

import autoset.model.*
import autoset.parsers.YamlParser
import utest.*

object YamlParserTest extends TestSuite:

  /** Parse `text`, returning the result and everything reported. */
  def parse(text: String): (Option[Obj], String) =
    val out = java.io.ByteArrayOutputStream()
    val reporter = Reporter(java.io.PrintStream(out))
    val in = java.io.ByteArrayInputStream(text.getBytes("utf-8"))
    val result = YamlParser.parse("a.yaml", in, text.length, reporter)
    assert(result.isEmpty == reporter.hasErrors)
    (result, out.toString)

  def check(text: String, expected: String, reported: String = "") =
    val (result, out) = parse(text.stripMargin)
    assert(out == reported.stripMargin)
    val actual = result.get.pretty(verbose = true)
    assert(actual == expected.stripMargin.trim)

  def checkError(text: String, expected: String) =
    val (result, out) = parse(text.stripMargin)
    assert(result.isEmpty)
    assert(out == expected.stripMargin)

  val tests = Tests {
    test("values") {
      check(
        """|str: hello world
           |quoted: "1"
           |num: 1.5e3
           |bool: true
           |null: ~
           |empty:
           |block: |
           |  a
           |  b
           |arr:
           |  - 1
           |  - x: y
           |flow: {a: [1, 2]}
           |""",
        """|{ // a.yaml:1:1
           |  str: "hello world", // a.yaml:1:6
           |  quoted: "1", // a.yaml:2:9
           |  num: "1.5e3", // a.yaml:3:6
           |  bool: "true", // a.yaml:4:7
           |  null: null, // a.yaml:5:7
           |  empty: null, // a.yaml:6:7
           |  block: "a\nb\n", // a.yaml:7:8
           |  arr: [ // a.yaml:11:3
           |    "1", // a.yaml:11:5
           |    { // a.yaml:12:5
           |      x: "y" // a.yaml:12:8
           |    }
           |  ],
           |  flow: { // a.yaml:13:7
           |    a: [ // a.yaml:13:11
           |      "1", // a.yaml:13:12
           |      "2" // a.yaml:13:15
           |    ]
           |  }
           |}"""
      )
    }
    test("kinds") {
      val (Some(o), _) = parse("s: a\nq: 'true'\nn: 0x1F\nb: false\nyes: yes"): @unchecked
      val kinds = o.fields.values.collect { case s: Str => s.kind }.toList
      assert(kinds == List(LitKind.String, LitKind.String, LitKind.Num, LitKind.Bool, LitKind.String))
    }
    test("byte offsets") {
      val (Some(o), _) = parse("é: 1\nb: 2"): @unchecked
      assert(o.fields("é").effectiveOrigin == Origin.File("a.yaml", 4, 1, 4))
      assert(o.fields("b").effectiveOrigin == Origin.File("a.yaml", 9, 2, 4))
    }
    test("empty values") {
      check(
        """|a:
           |b:
           |  -
           |  - 1
           |""",
        """|{ // a.yaml:1:1
           |  a: null, // a.yaml:1:3
           |  b: [ // a.yaml:3:3
           |    null, // a.yaml:3:4
           |    "1" // a.yaml:4:5
           |  ]
           |}"""
      )
    }
    test("empty file") {
      check("# nothing here\n", "{} // a.yaml:1:1")
      check("", "{} // a.yaml:1:1")
    }
    test("duplicate keys") {
      check(
        """|a: 1
           |a: 2
           |""",
        """|{ // a.yaml:1:1
           |  a: "2" // a.yaml:2:4
           |}""",
        """|warning: a.yaml:2:1: duplicate key 'a', the last one takes precedence
           |a: 2
           |^
           |"""
      )
    }
    test("errors") {
      test("syntax") {
        checkError(
          """|a:
             |  b: 1
             | c: 2
             |""",
          """|error: a.yaml:3:2: Entries within the same map must start at the same column.
             | c: 2
             | ^
             |"""
        )
      }
      test("unterminated") {
        checkError(
          """|a: "x
             |b: 2
             |""",
          """|error: a.yaml:1:4: Expected closing " but reached EOF
             |a: "x
             |   ^
             |"""
        )
      }
      test("unclosed flow collection") {
        checkError(
          """|a: 1
             |b: [1,
             |""",
          """|error: a.yaml:2:4: Expected ']' to close this flow collection, but reached EOF
             |b: [1,
             |   ^
             |"""
        )
      }
      test("end of file") {
        checkError(
          """|a: 1
             |b: {c:
             |""",
          """|error: a.yaml:3:1: Expected a value, but reached EOF
             |"""
        )
      }
      test("not a map") {
        checkError(
          """|# list
             |- 1
             |""",
          """|error: a.yaml:2:1: expected a top-level YAML map
             |- 1
             |^
             |"""
        )
      }
      test("multiple documents") {
        val (result, out) = parse("a: 1\n---\nb: 2\n")
        assert(result.isEmpty)
        assert(out.startsWith("error: a.yaml:2:1: Expected a single document"))
      }
    }
  }
