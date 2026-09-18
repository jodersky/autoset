package test

import autoset.model.*
import utest.*

object ReporterTest extends TestSuite:

  /** Report with `f`, returning the reporter and its rendered diagnostics. */
  def capture(f: Reporter => Unit): (Reporter, String) =
    val reporter = Reporter()
    f(reporter)
    (reporter, reporter.render)

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
    test("structured") {
      val pos = Origin.File("app.ini", 10, 2, 5)
      val (r, _) = capture { r =>
        r.warn("first")
        r.error("second", pos, "a b = c")
      }
      assert(
        r.diagnostics == List(
          Diagnostic(Severity.Warning, "first"),
          Diagnostic(Severity.Error, "second", Some(pos), "a b = c")
        )
      )
      assert(r.diagnostics(1).col == 5)
    }
    test("collecting doesn't print") {
      val out = java.io.ByteArrayOutputStream()
      val err = System.err
      System.setErr(java.io.PrintStream(out))
      try Reporter().error("boom")
      finally System.setErr(err)
      assert(out.toString == "")
    }
    test("print") {
      val reporter = Reporter()
      reporter.warn("a")
      reporter.error("b")
      val out = java.io.ByteArrayOutputStream()
      reporter.print(java.io.PrintStream(out))
      assert(out.toString == "warning: a\nerror: b\n")
    }
    test("printing") {
      // diagnostics are printed as soon as they are reported
      val out = java.io.ByteArrayOutputStream()
      val reporter = Reporter.printing(java.io.PrintStream(out))
      reporter.warn("a")
      assert(out.toString == "warning: a\n")
      reporter.error("b")
      assert(out.toString == "warning: a\nerror: b\n")
      assert(reporter.diagnostics.size == 2, reporter.errors == 1, reporter.warnings == 1)
    }
    test("onReport") {
      val seen = collection.mutable.ListBuffer.empty[Diagnostic]
      val reporter = Reporter(seen += _)
      reporter.error("x")
      assert(seen.toList == reporter.diagnostics)
    }
  }
