package configparse.model

enum Origin:
  case File(file: String, row: Int, col: Int)
  case Env(name: String)
  case Props(name: String)
  case Arg()
  case Code(file: String, row: Int, col: Int)

  def pretty = this match
    case File(file, row, col) =>
      val b = StringBuilder()
      b ++= file
      if row >= 0 then
        b += ':'
        b ++= row.toString
      if col >= 0 then
        b += ':'
        b ++= col.toString
      b.result()
    case Env(name) =>
      s"env $name"
    case Props(name) =>
      s"system property $name"
    case Arg() =>
      s"arg"
    case Code(file, row, col) =>
      s"source $file:$row:$col"
