package test

import autoset.model.*
import autoset.parsers.HoconParser
import utest.*

object HoconParserTest extends TestSuite:

  /** Parse `text`, returning the result and everything reported. */
  def parse(text: String): (Option[Obj], String) =
    val out = java.io.ByteArrayOutputStream()
    val reporter = Reporter(java.io.PrintStream(out))
    val in = java.io.ByteArrayInputStream(text.getBytes("utf-8"))
    val result = HoconParser.parse("a.conf", in, text.length, reporter)
    assert(result.isEmpty == reporter.hasErrors)
    (result, out.toString)

  def check(text: String, expected: String) =
    val (result, out) = parse(text.stripMargin)
    assert(out == "")
    val actual = result.get.pretty(verbose = true)
    assert(actual == expected.stripMargin.trim)

  def checkError(text: String, expected: String) =
    val (result, out) = parse(text.stripMargin)
    assert(result.isEmpty)
    assert(out == expected.stripMargin)

  val tests = Tests {
    test("values") {
      check(
        """|str = "hello"
           |unquoted = hello world
           |num = 1.5
           |bool = true
           |null = null
           |arr = [1, a, {b: c}]
           |obj {
           |  a.b = 1
           |}
           |""",
        """|{ // a.conf:1
           |  str: "hello", // a.conf:1
           |  unquoted: "hello world", // a.conf:2
           |  num: "1.5", // a.conf:3
           |  bool: "true", // a.conf:4
           |  null: null, // a.conf:5
           |  arr: [ // a.conf:6
           |    "1", // a.conf:6
           |    "a", // a.conf:6
           |    { // a.conf:6
           |      b: "c" // a.conf:6
           |    }
           |  ],
           |  obj: { // a.conf:7
           |    a: { // a.conf:8
           |      b: "1" // a.conf:8
           |    }
           |  }
           |}"""
      )
    }
    test("kinds") {
      val (Some(o), _) = parse("s = \"1\"\nu = x\nn = 1\nb = false"): @unchecked
      val kinds = o.fields.values.collect { case s: Str => s.kind }.toList
      assert(kinds == List(LitKind.String, LitKind.String, LitKind.Num, LitKind.Bool))
    }
    test("key order follows source") {
      val (Some(o), _) = parse("z = 1\na = 2\nm = 3\nb { y = 1\nx = 2 }"): @unchecked
      assert(o.fields.keys.toList == List("z", "a", "m", "b"))
      assert(o.fields("b").asInstanceOf[Obj].fields.keys.toList == List("y", "x"))
      // keys on the same line can't be told apart, so they are sorted
      val (Some(o2), _) = parse("z = 1, a = 2"): @unchecked
      assert(o2.fields.keys.toList == List("a", "z"))
    }
    test("line origins have byte offsets") {
      val (Some(o), _) = parse("é = 1\nb = 2"): @unchecked
      assert(o.fields("b").effectiveOrigin == Origin.File("a.conf", 7, 2, -1))
    }
    test("merging and substitutions") {
      check(
        """|base { a = 1, b = 2 }
           |base { b = 3 }
           |ref = ${base.a}
           |opt = ${?nope}
           |""",
        """|{ // a.conf:1
           |  base: { // a.conf:1
           |    a: "1", // a.conf:1
           |    b: "3" // a.conf:2
           |  },
           |  ref: "1" // a.conf:1
           |}"""
      )
    }
    test("environment is not used") {
      checkError(
        "home = ${HOME}\n",
        """|error: a.conf:1: Could not resolve substitution to a value: ${HOME}
           |home = ${HOME}
           |"""
      )
    }
    test("errors") {
      checkError(
        """|a = 1
           |b = {
           |""",
        """|error: a.conf:3: expecting a close parentheses ')' here, not: end of file
           |"""
      )
      checkError(
        """|a = 1
           |b = ]
           |c = 2
           |""",
        """|error: a.conf:2: Expecting a value but got wrong token: ']' (if you intended ']' to be part of a key or string value, try enclosing the key or value in double quotes, or you may be able to rename the file .properties rather than .conf)
           |b = ]
           |"""
      )
    }
  }
