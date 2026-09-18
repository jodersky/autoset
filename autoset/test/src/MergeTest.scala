package test

import autoset.model.*
import utest.*
import Helpers.*

object MergeTest extends TestSuite:

  val tests = Tests {
    test("override") {
      val o1 = obj(file(1))(
        "a" -> str("ok", file(2)),
        "b" -> str("false", file(3))
      )
      val o2 = obj(env(""))(
        "a" -> str("override", env("A")),
        "c" -> str("false", env("C"))
      )
      merge(o1, o2)
      assert(
        o1 == obj(env(""), file(1))(
          "a" -> str("override", env("A"), file(2)),
          "b" -> str("false", file(3)),
          "c" -> str("false", env("C"))
        )
      )
    }
    test("nested") {
      val o1 = obj(file(1))(
        "a" -> str("ok", file(2)),
        "b" -> obj(file(3))(
          "a" -> obj(file(4))(
            "a" -> str("ok", file(5)),
            "b" -> str("ok", file(6))
          )
        )
      )
      val o2 = obj(file(1, "b.conf"))(
        "b" -> obj(file(2, "b.conf"))(
          "a" -> obj(file(3, "b.conf"))(
            "a" -> str("override", file(4, "b.conf")),
            "c" -> str("ok", file(5, "b.conf"))
          ),
          "b" -> str("b", file(6, "b.conf"))
        )
      )
      merge(o1, o2)
      assert(
        o1 == obj(file(1, "b.conf"), file(1))(
          "a" -> str("ok", file(2)),
          "b" -> obj(file(2, "b.conf"), file(3))(
            "a" -> obj(file(3, "b.conf"), file(4))(
              "a" -> str("override", file(4, "b.conf"), file(5)),
              "b" -> str("ok", file(6)),
              "c" -> str("ok", file(5, "b.conf"))
            ),
            "b" -> str("b", file(6, "b.conf"))
          )
        )
      )
    }
    test("precedence") {
      // later merges take precedence; origins list the full history
      val root = obj(file(1))("a" -> str("file", file(2)), "b" -> str("file", file(3)))
      merge(root, obj(env(""))("a" -> str("env", env("A")), "b" -> str("env", env("B"))))
      merge(root, obj(props(""))("b" -> str("props", props("b"))))
      assert(
        root == obj(props(""), env(""), file(1))(
          "a" -> str("env", env("A"), file(2)),
          "b" -> str("props", props("b"), env("B"), file(3))
        )
      )
    }
    test("history carried over") {
      // a value that already overrode something keeps its history when merged
      val o1 = obj(file(1))("a" -> str("1", file(2)))
      val o2 = obj(env(""))("a" -> str("3", env("A"), env("A_OLD")))
      merge(o1, o2)
      assert(o1.fields("a") == str("3", env("A"), env("A_OLD"), file(2)))
      assert(o1.fields("a").effectiveOrigin == env("A"))
    }
    test("object replaces leaf") {
      val o1 = obj(file(1))("a" -> str("b", file(2)))
      val o2 = obj(env(""))("a" -> obj(env("A"))())
      merge(o1, o2)
      assert(o1 == obj(env(""), file(1))("a" -> obj(env("A"), file(2))()))
    }
    test("leaf replaces object") {
      val o1 = obj(file(1))("a" -> obj(file(2))("b" -> str("c", file(3))))
      val o2 = obj(env(""))("a" -> nul(env("A")))
      merge(o1, o2)
      assert(o1 == obj(env(""), file(1))("a" -> nul(env("A"), file(2))))
    }
    test("arrays are replaced") {
      val o1 = obj(file(1))("a" -> arr(file(2))(str("1", file(2)), str("2", file(2))))
      val o2 = obj(env(""))("a" -> arr(env("A"))(str("3", env("A"))))
      merge(o1, o2)
      assert(o1 == obj(env(""), file(1))("a" -> arr(env("A"), file(2))(str("3", env("A")))))
    }
    test("key order") {
      // existing keys keep their position, new keys are appended
      val o1 = obj(file(1))("a" -> str("1", file(1)), "b" -> str("2", file(2)))
      val o2 = obj(env(""))("c" -> str("3", env("C")), "a" -> str("4", env("A")))
      merge(o1, o2)
      assert(o1.fields.keys.toList == List("a", "b", "c"))
    }
    test("empty") {
      val o1 = obj(file(1))("a" -> str("1", file(2)))
      merge(o1, obj(env(""))())
      assert(o1 == obj(env(""), file(1))("a" -> str("1", file(2))))
    }
    test("type change warnings") {
      def warnings(o1: Obj, o2: Obj): String =
        val out = java.io.ByteArrayOutputStream()
        autoset.merge(o1, o2, Reporter(java.io.PrintStream(out)))
        out.toString
      test("object replaced by value") {
        val out = warnings(
          obj(file(1))("a" -> obj(file(2))("b" -> obj(file(3))("c" -> str("1", file(3))))),
          obj(env(""))("a" -> obj(env("A"))("b" -> str("x", env("A_B"))))
        )
        assert(out == "warning: env A_B: 'a.b' is set to a value, replacing an object from app.conf:3:1\n")
      }
      test("value replaced by object") {
        val out = warnings(
          obj(file(1))("a" -> arr(file(2))()),
          obj(env(""))("a" -> obj(env("A_B"))("b" -> str("x", env("A_B"))))
        )
        assert(out == "warning: env A_B: 'a' is set to an object, replacing a list from app.conf:2:1\n")
      }
      test("no warnings") {
        // values of different types, and nulls, are replaced silently
        val out = warnings(
          obj(file(1))("a" -> arr(file(2))(), "b" -> nul(file(3)), "c" -> obj(file(4))()),
          obj(env(""))("a" -> str("x", env("A")), "b" -> obj(env("B"))(), "c" -> nul(env("C")))
        )
        assert(out == "")
      }
    }
  }
