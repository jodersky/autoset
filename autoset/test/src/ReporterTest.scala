package test

import autoset.model.*
import utest.*

object ReporterTest extends TestSuite:

  def capture(f: Reporter => Unit): (Reporter, String) =
    val out = java.io.ByteArrayOutputStream()
    val reporter = Reporter(java.io.PrintStream(out))
    f(reporter)
    (reporter, out.toString)

  val tests = Tests {
    test("plain") {
      val (r, out) = capture(_.error("boom"))
      assert(out == "error: boom\n")
      assert(r.hasErrors, r.errors == 1, r.warnings == 0)
    }
    test("position and caret") {
      val (r, out) = capture(
        _.error("unexpected '='", Origin.File("app.ini", 10, 2, 5), "a b = c")
      )
      assert(out == "error: app.ini:2:5: unexpected '='\na b = c\n    ^\n")
    }
    test("warning") {
      val (r, out) = capture(_.warn("key without value", Origin.File("app.ini", 0, 3, -1), "a"))
      assert(out == "warning: app.ini:3: key without value\na\n")
      assert(!r.hasErrors, r.warnings == 1)
    }
    test("unknown position") {
      val (_, out) = capture(_.error("bad", Origin.File("app.props", -1, -1, -1)))
      assert(out == "error: app.props: bad\n")
    }
  }
