package autoset.types

/** A number at a scale, e.g. `Quantity(1.5, "G")` or `Quantity(4, "Mi")`.
  *
  * A quantity has no dimension of its own: what it counts — bytes, requests,
  * cores — is for whoever declares it to say. The scale is kept as it was
  * given, so that a quantity which is read from a configuration and shown
  * again reads the same.
  */
case class Quantity(value: Double, scale: Quantity.Scale):

  /** This quantity at `scale`, e.g. `Quantity(1.5, "G").toScale("M")` is
    * 1500.
    *
    * Every scale is a power of ten or of two, so the conversion itself is
    * exact; only its result is rounded, once, to the nearest `Double`.
    */
  def toScale(scale: Quantity.Scale = ""): Double =
    if scale == this.scale then value
    else (BigDecimal(value) * Quantity.factor(this.scale) / Quantity.factor(scale)).toDouble

object Quantity:

  /** The scale of a [[Quantity]]: an SI prefix from femto to peta, taken
    * every third order of magnitude, or one of the binary prefixes.
    *
    * Case matters, as it does in SI: `m` is milli and `M` is mega, `p` is
    * pico and `P` is peta.
    */
  type Scale =
    "f" | "p" | "n" | "u" | "m" | "" |
      "k" | "M" | "G" | "T" | "P" |
      "Ki" | "Mi" | "Gi" | "Ti" | "Pi"

  /** Every scale, from the smallest to the largest. */
  val scales: List[Scale] =
    List("f", "p", "n", "u", "m", "", "k", "Ki", "M", "Mi", "G", "Gi", "T", "Ti", "P", "Pi")

  /** What one at `scale` is at scale `""`, e.g. 1000 for `k` and 1024 for
    * `Ki`. It is exact: every scale is a power of ten or of two.
    */
  def factor(scale: Scale): BigDecimal = scale match
    case "f" => tenTo(-15)
    case "p" => tenTo(-12)
    case "n" => tenTo(-9)
    case "u" => tenTo(-6)
    case "m" => tenTo(-3)
    case "" => BigDecimal(1)
    case "k" => tenTo(3)
    case "M" => tenTo(6)
    case "G" => tenTo(9)
    case "T" => tenTo(12)
    case "P" => tenTo(15)
    case "Ki" => twoTo(10)
    case "Mi" => twoTo(20)
    case "Gi" => twoTo(30)
    case "Ti" => twoTo(40)
    case "Pi" => twoTo(50)

  // `BigDecimal.pow` takes no negative exponent, and a negative power of ten
  // is a decimal of one digit, so dividing by the positive one is exact
  private def tenTo(exponent: Int): BigDecimal =
    if exponent >= 0 then BigDecimal(10).pow(exponent)
    else BigDecimal(1) / BigDecimal(10).pow(-exponent)

  private def twoTo(exponent: Int): BigDecimal = BigDecimal(2).pow(exponent)

  /** The scale named `name`, if there is one. */
  def scale(name: String): Option[Scale] = scales.find(_ == name)
