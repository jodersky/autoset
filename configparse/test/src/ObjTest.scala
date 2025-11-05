package test

import configparse.*
import utest.*

object ObjTest extends TestSuite:

  val tests = Tests{
    test("merge") {
      val o1 = Config(
        "a" -> Str("ok"),
        "b" -> Str("false")
      )
      val o2 = Config(
        "a" -> Str("override"),
        "c" -> Str("false")
      )

      o1.mergeFrom(o2)

      assert(
        o1 == Config(
          "a" -> Str("override"),
          "b"-> Str("false"),
          "c" -> Str("false")
        )
      )
    }
    test("merge nested") {
      val o1 = Config(
        "a" -> Str("ok"),
        "b" -> Config(
          "a" -> Config(
            "a" -> Str("ok"),
            "b" -> Str("ok")
          )
        )
      )
      val o2 = Config(
        "b" -> Config(
          "a" -> Config(
            "a" -> Str("override"),
            "c" -> Str("ok")
          ),
          "b" -> Null()
        )
      )

      o1.mergeFrom(o2)

      assert(
        o1 == Config(
          "a" -> Str("ok"),
          "b" -> Config(
            "a" -> Config(
              "a" -> Str("override"),
              "b" -> Str("ok"),
              "c" -> Str("ok")
            ),
            "b" -> Null()
          )
        )
      )
    }
    test("empty") {
      val o1 = Config(
        "a" -> Config()
      )
      val o2 = Config(
        "a" -> Null()
      )
      o1.mergeFrom(o2)
      assert(
        o1 == Config(
          "a" -> Config()
        )
      )
    }
  }
