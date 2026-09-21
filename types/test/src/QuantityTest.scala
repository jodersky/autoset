package test

import autoset.types.Quantity
import utest.*

object QuantityTest extends TestSuite:

  val tests = Tests {
    test("scales are ordered from the smallest to the largest") {
      val factors = Quantity.scales.map(s => Quantity(1, s).toScale(""))
      assert(factors == factors.sorted)
      assert(Quantity(1, "").toScale("") == 1.0)
      assert(Quantity(1, "k").toScale("") == 1000.0)
      assert(Quantity(1, "Ki").toScale("") == 1024.0)
      assert(Quantity(1, "Pi").toScale("") == 1125899906842624.0)
      assert(Quantity(1, "f").toScale("") == 1e-15)
    }
    test("scale by name") {
      assert(Quantity.scale("Mi") == Some("Mi"))
      assert(Quantity.scale("") == Some(""))
      // case matters, as it does in SI
      assert(Quantity.scale("m") == Some("m"))
      assert(Quantity.scale("M") == Some("M"))
      assert(Quantity.scale("mi").isEmpty)
      assert(Quantity.scale("KI").isEmpty)
      // outside femto to peta
      assert(Quantity.scale("E").isEmpty)
      assert(Quantity.scale("a").isEmpty)
      // the orders of magnitude which SI has but this skips
      for skipped <- List("c", "d", "da", "h") do assert(Quantity.scale(skipped).isEmpty)
    }
    test("to the same scale") {
      for s <- Quantity.scales do assert(Quantity(7, s).toScale(s) == 7.0)
    }
    test("decimal scales") {
      assert(Quantity(1.5, "G").toScale("M") == 1500.0)
      assert(Quantity(500, "m").toScale("") == 0.5)
      assert(Quantity(1, "k").toScale("m") == 1000000.0)
      assert(Quantity(2, "P").toScale("T") == 2000.0)
    }
    test("binary scales") {
      assert(Quantity(1, "Ki").toScale("") == 1024.0)
      assert(Quantity(4, "Mi").toScale("") == 4194304.0)
      assert(Quantity(1024, "Ki").toScale("Mi") == 1.0)
      // the same conversion pkl gives for 1.6.gib.toUnit("tib")
      assert(Quantity(1.6, "Gi").toScale("Ti") == 0.0015625)
    }
    test("milli is not mega, pico is not peta") {
      assert(Quantity(1, "m").toScale("") == 0.001)
      assert(Quantity(1, "M").toScale("") == 1000000.0)
      assert(Quantity(1, "p").toScale("f") == 1000.0)
      assert(Quantity(1, "P").toScale("T") == 1000.0)
    }
    test("the full span of scales") {
      assert(Quantity(1, "P").toScale("f") == 1e30)
      assert(Quantity(1, "f").toScale("P") == 1e-30)
    }
    test("between binary scales") {
      assert(Quantity(1.6, "Gi").toScale("Ti") == 0.0015625)
      assert(Quantity(0.1, "Ki").toScale("") == 102.4)
      assert(Quantity(1, "Pi").toScale("Ki") == 1099511627776.0)
    }
    test("the conversion is exact, and rounds once at the end") {
      assert(Quantity(500, "m").toScale("") == 0.5)
      assert(Quantity(1, "p").toScale("f") == 1000.0)
      assert(Quantity(1, "P").toScale("f") == 1e30)
      assert(Quantity(1, "f").toScale("P") == 1e-30)
      // crossing between a binary and a decimal scale is exact too, where
      // pkl, which converts through doubles throughout, gives
      // 0.0011811160064000002 for the same conversion
      assert(Quantity(1.1, "Gi").toScale("T") == 0.0011811160064)
    }
    test("no scale converts to the base scale") {
      assert(Quantity(4, "Ki").toScale() == 4096.0)
      assert(Quantity(1.5, "G").toScale() == 1500000000.0)
    }
  }
