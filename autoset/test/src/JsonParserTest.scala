package test

import autoset.model.*
import autoset.parsers.JsonParser
import utest.*

object JsonParserTest extends TestSuite:

  /** Parse `text`, returning the result and everything reported. */
  def parse(text: String): (Option[Obj], String) =
    val out = java.io.ByteArrayOutputStream()
    val reporter = Reporter.printing(java.io.PrintStream(out))
    val in = java.io.ByteArrayInputStream(text.getBytes("utf-8"))
    val result = JsonParser.parse("a.json", in, text.length, reporter)
    assert(result.isEmpty == reporter.hasErrors)
    (result, out.toString)

  def check(text: String, expected: String) =
    val (result, out) = parse(text)
    assert(out == "")
    val actual = result.get.pretty(verbose = true)
    assert(actual == expected.stripMargin.trim)

  def checkError(text: String, expected: String) =
    val (result, out) = parse(text)
    assert(result.isEmpty)
    assert(out == expected.stripMargin)

  val tests = Tests {
    test("values") {
      check(
        """|{
           |  "str": "hello",
           |  "num": 1.5e3,
           |  "int": -2,
           |  "bool": true,
           |  "null": null,
           |  "arr": [1, "a"],
           |  "obj": {"a": {}}
           |}""".stripMargin,
        """|{ // a.json:1:1
           |  str: "hello", // a.json:2:10
           |  num: "1.5e3", // a.json:3:10
           |  int: "-2", // a.json:4:10
           |  bool: "true", // a.json:5:11
           |  null: null, // a.json:6:11
           |  arr: [ // a.json:7:10
           |    "1", // a.json:7:11
           |    "a" // a.json:7:14
           |  ],
           |  obj: { // a.json:8:10
           |    a: {} // a.json:8:16
           |  }
           |}"""
      )
    }
    test("kinds") {
      val (Some(o), _) = parse("""{"s": "1", "n": 1, "t": true, "f": false}"""): @unchecked
      val kinds = o.fields.values.collect { case s: Str => s.kind }.toList
      assert(kinds == List(LitKind.String, LitKind.Num, LitKind.Bool, LitKind.Bool))
    }
    test("byte offsets") {
      val (Some(o), _) = parse("{\n  \"a\": 1\n}"): @unchecked
      assert(o.fields("a").effectiveOrigin == Origin.File("a.json", 9, 2, 8))
    }
    test("columns count characters") {
      // "é" is two bytes in UTF-8, but one column
      val (Some(o), _) = parse("""{"é": 1}"""): @unchecked
      assert(o.fields("é").effectiveOrigin == Origin.File("a.json", 7, 1, 7))
    }
    test("crlf") {
      check(
        "{\r\n\"a\": 1\r\n}",
        """|{ // a.json:1:1
           |  a: "1" // a.json:2:6
           |}"""
      )
    }
    test("duplicate keys") {
      val (result, out) = parse("{\n\"a\": 1,\n\"a\": 2\n}")
      assert(result.get.fields("a").asInstanceOf[Str].raw == "2")
      assert(out == "warning: a.json:3:1: duplicate key 'a', the last one takes precedence\n\"a\": 2\n^\n")
    }
    test("errors") {
      test("syntax") {
        checkError(
          "{\n  \"a\": 1,\n  \"b\" 2\n}",
          """|error: a.json:3:7: expected : got "2"
             |  "b" 2
             |      ^
             |"""
        )
      }
      test("not an object") {
        checkError(
          "\n[1, 2]",
          """|error: a.json:2:1: expected a top-level JSON object
             |[1, 2]
             |^
             |"""
        )
      }
      test("unexpected end") {
        checkError(
          "{\n\"a\": [1,",
          """|error: a.json:2:9: unexpected end of file
             |"a": [1,
             |        ^
             |"""
        )
      }
      test("empty") {
        checkError("", "error: a.json:1:1: unexpected end of file\n")
      }
    }
  }
