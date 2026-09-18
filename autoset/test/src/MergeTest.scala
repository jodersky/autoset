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
      o1.mergeFrom(o2)
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
      o1.mergeFrom(o2)
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
      root.mergeFrom(obj(env(""))("a" -> str("env", env("A")), "b" -> str("env", env("B"))))
      root.mergeFrom(obj(props(""))("b" -> str("props", props("b"))))
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
      o1.mergeFrom(o2)
      assert(o1.fields("a") == str("3", env("A"), env("A_OLD"), file(2)))
      assert(o1.fields("a").effectiveOrigin == env("A"))
    }
    test("object replaces leaf") {
      val o1 = obj(file(1))("a" -> str("b", file(2)))
      val o2 = obj(env(""))("a" -> obj(env("A"))())
      o1.mergeFrom(o2)
      assert(o1 == obj(env(""), file(1))("a" -> obj(env("A"), file(2))()))
    }
    test("leaf replaces object") {
      val o1 = obj(file(1))("a" -> obj(file(2))("b" -> str("c", file(3))))
      val o2 = obj(env(""))("a" -> nul(env("A")))
      o1.mergeFrom(o2)
      assert(o1 == obj(env(""), file(1))("a" -> nul(env("A"), file(2))))
    }
    test("arrays are replaced") {
      val o1 = obj(file(1))("a" -> arr(file(2))(str("1", file(2)), str("2", file(2))))
      val o2 = obj(env(""))("a" -> arr(env("A"))(str("3", env("A"))))
      o1.mergeFrom(o2)
      assert(o1 == obj(env(""), file(1))("a" -> arr(env("A"), file(2))(str("3", env("A")))))
    }
    test("key order") {
      // existing keys keep their position, new keys are appended
      val o1 = obj(file(1))("a" -> str("1", file(1)), "b" -> str("2", file(2)))
      val o2 = obj(env(""))("c" -> str("3", env("C")), "a" -> str("4", env("A")))
      o1.mergeFrom(o2)
      assert(o1.fields.keys.toList == List("a", "b", "c"))
    }
    test("empty") {
      val o1 = obj(file(1))("a" -> str("1", file(2)))
      o1.mergeFrom(obj(env(""))())
      assert(o1 == obj(env(""), file(1))("a" -> str("1", file(2))))
    }
  }
