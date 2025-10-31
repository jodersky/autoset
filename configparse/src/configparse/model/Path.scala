package configparse.model

case class Path(segments: Vector[String] = Vector()):
  override def toString(): String = segments.mkString(".")

  def /(segment: String): Path = Path(segments :+ segment)

object Path:
  given Conversion[Iterable[String], Path] = ls => Path(ls.toVector)
  given Conversion[String, Path] = s => Path(s.split('.').toVector)
