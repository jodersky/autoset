package configparse.model

type Path = Seq[String]
object Path:

  val Empty = Vector.empty[String]

  def split(str: String): Path =
    str.split('.').toVector
