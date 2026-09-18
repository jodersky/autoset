package test

import autoset.model.*
import autoset.parsers.IniParser
import utest.*

object IniParserTest extends TestSuite:

  /** Parse `text`, returning the result and everything reported. */
  def parse(text: String): (Option[Obj], String) =
    val out = java.io.ByteArrayOutputStream()
    val reporter = Reporter.printing(java.io.PrintStream(out))
    val in = java.io.ByteArrayInputStream(text.getBytes("utf-8"))
    val result = IniParser.parse("a.ini", in, text.length, reporter)
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
        """|a = 1
           |b=hello world
           |# comment
           |; comment
           |c =
           |""",
        """|{ // a.ini:1:1
           |  a: "1", // a.ini:1:5
           |  b: "hello world" // a.ini:2:3
           |}"""
      )
    }
    test("kind is unknown") {
      val (Some(o), _) = parse("a = true"): @unchecked
      assert(o.fields("a") == Str("true", LitKind.Unknown, List(Origin.File("a.ini", 4, 1, 5))))
    }
    test("sections") {
      check(
        """|a = 1
           |[x.y]
           |b = 2
           |[z]
           |c = 3
           |""",
        """|{ // a.ini:1:1
           |  a: "1", // a.ini:1:5
           |  x: { // a.ini:2:1
           |    y: { // a.ini:2:1
           |      b: "2" // a.ini:3:5
           |    }
           |  },
           |  z: { // a.ini:4:1
           |    c: "3" // a.ini:5:5
           |  }
           |}"""
      )
    }
    test("reopened sections") {
      check(
        """|[x.y]
           |a = 1
           |[z]
           |[x]
           |b = 2
           |[x.y]
           |c = 3
           |""",
        """|{ // a.ini:1:1
           |  x: { // a.ini:6:1, a.ini:4:1, a.ini:1:1
           |    y: { // a.ini:6:1, a.ini:1:1
           |      a: "1", // a.ini:2:5
           |      c: "3" // a.ini:7:5
           |    },
           |    b: "2" // a.ini:5:5
           |  },
           |  z: {} // a.ini:3:1
           |}"""
      )
    }
    test("duplicate key") {
      check(
        """|a = 1
           |a = 2
           |""",
        """|{ // a.ini:1:1
           |  a: "2" // a.ini:2:5
           |}""",
        """|warning: a.ini:2:1: duplicate key 'a', the last one takes precedence
           |a = 2
           |^
           |"""
      )
    }
    test("section replaces key") {
      check(
        """|a = 1
           |[a]
           |b = 2
           |""",
        """|{ // a.ini:1:1
           |  a: { // a.ini:2:1
           |    b: "2" // a.ini:3:5
           |  }
           |}""",
        """|warning: a.ini:2:1: section [a] replaces key 'a'
           |[a]
           |^
           |"""
      )
    }
    test("unicode") {
      // "é" is two bytes in UTF-8, but one column
      check(
        """|[é]
           |é = é
           |x = 1
           |""",
        """|{ // a.ini:1:1
           |  é: { // a.ini:1:1
           |    é: "é", // a.ini:2:5
           |    x: "1" // a.ini:3:5
           |  }
           |}"""
      )
      val (Some(o), _) = parse("[a]\nk = é\nx = 1"): @unchecked
      val x = o.fields("a").asInstanceOf[Obj].fields("x")
      assert(x.effectiveOrigin == Origin.File("a.ini", 15, 3, 5))
      checkError(
        "é = é\n[",
        """|error: a.ini:2:2: Expected alphanumeric or '_' or '-'. Found EOF.
           |[
           | ^
           |"""
      )
    }
    test("errors") {
      checkError(
        """|[a]
           |[a]=a
           |""",
        """|error: a.ini:2:4: Expected alphanumeric or '_' or '-'. Found '='.
           |[a]=a
           |   ^
           |"""
      )
      checkError(
        """|a = 1
           |b c
           |""",
        """|error: a.ini:2:3: Expected '='. Found 'c'.
           |b c
           |  ^
           |"""
      )
    }
  }
