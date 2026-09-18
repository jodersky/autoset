package test

import autoset.model.*
import autoset.parsers.PropsParser
import utest.*

object PropsParserTest extends TestSuite:

  /** Parse `text`, returning the result and everything reported. */
  def parse(text: String): (Option[Obj], String) =
    val out = java.io.ByteArrayOutputStream()
    val reporter = Reporter.printing(java.io.PrintStream(out))
    val in = java.io.ByteArrayInputStream(text.getBytes("utf-8"))
    val result = PropsParser.parse("a.properties", in, text.length, reporter)
    assert(result.isEmpty == reporter.hasErrors)
    (result, out.toString)

  def check(text: String, expected: String, reported: String = "") =
    val (result, out) = parse(text.stripMargin)
    assert(out == reported.stripMargin)
    val actual = result.get.pretty()
    assert(actual == expected.stripMargin.trim)

  val tests = Tests {
    test("values") {
      check(
        """|# comment
           |global_key=a
           |section1.a=1
           |section2.a : 2
           |section1.b.c hello \
           |   world
           |unicode=éé
           |empty=
           |""",
        """|{ // a.properties
           |  global_key: "a",
           |  section1: {
           |    a: "1",
           |    b: {
           |      c: "hello world"
           |    }
           |  },
           |  section2: {
           |    a: "2"
           |  },
           |  unicode: "éé",
           |  empty: ""
           |}"""
      )
    }
    test("origin and kind") {
      val (Some(o), _) = parse("a=1"): @unchecked
      assert(o.fields("a") == Str("1", LitKind.Unknown, List(Origin.File("a.properties", -1, -1, -1))))
    }
    test("conflicts") {
      check(
        """|a=1
           |a=2
           |b=1
           |b.c=2
           |d.e=1
           |d=2
           |""",
        """|{ // a.properties
           |  a: "2",
           |  b: {
           |    c: "2"
           |  },
           |  d: "2"
           |}""",
        """|warning: a.properties: duplicate key 'a', the last one takes precedence
           |warning: a.properties: key 'b.c' replaces key 'b'
           |warning: a.properties: key 'd' replaces its sub-keys
           |"""
      )
    }
    test("empty segments") {
      check(
        """|a..b=1
           |.c=2
           |d=3
           |""",
        """|{ // a.properties
           |  d: "3"
           |}""",
        """|warning: a.properties: ignoring key 'a..b', which has an empty segment
           |warning: a.properties: ignoring key '.c', which has an empty segment
           |"""
      )
    }
    test("malformed escape") {
      val (result, out) = parse("a=\\u00zz")
      assert(result.isEmpty)
      assert(out.startsWith("error: a.properties: "))
    }
  }
