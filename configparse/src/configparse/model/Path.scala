package configparse.model

case class Path(segments: Seq[String]):
  override def toString: String = segments.mkString(".")

  def /(segment: String): Path = segments :+ segment

object Path:

  val Empty: Path = Path(Seq.empty)

  def apply(first: String, segments: String*): Path = Path(
    first +: segments.toSeq
  )

  def split(str: String): Path = str.split('.').toVector

  // using old-style implicit conversions until into is no longer experimental
  import scala.language.implicitConversions
  implicit def fromSeq(segments: Seq[String]): Path = Path(segments)
  implicit def fromString(str: String): Path = Path.split(str)
