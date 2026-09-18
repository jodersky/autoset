package autoset.model

/** Collects warnings and errors, printing them as they are reported. */
class Reporter(val stream: java.io.PrintStream = System.err):
  private var _warnings: Int = 0
  private var _errors: Int = 0

  def warnings: Int = _warnings
  def errors: Int = _errors
  def hasErrors: Boolean = _errors > 0

  def warn(message: String): Unit =
    print("warning", message, None, "")
    _warnings += 1

  /** @param line the source line at `pos`, shown with a caret under the column */
  def warn(message: String, pos: Origin, line: String = ""): Unit =
    print("warning", message, Some(pos), line)
    _warnings += 1

  def error(message: String): Unit =
    print("error", message, None, "")
    _errors += 1

  /** @param line the source line at `pos`, shown with a caret under the column */
  def error(message: String, pos: Origin, line: String = ""): Unit =
    print("error", message, Some(pos), line)
    _errors += 1

  private def print(
      level: String,
      message: String,
      pos: Option[Origin],
      line: String
  ): Unit =
    stream.print(level)
    stream.print(": ")
    pos.foreach(p => stream.print(p.pretty + ": "))
    stream.println(message)
    val col = pos match
      case Some(Origin.File(_, _, _, col, _)) => col
      case Some(Origin.Code(_, _, _, col)) => col
      case _ => -1
    if line.nonEmpty then
      stream.println(line)
      if col > 0 then stream.println(" " * (col - 1) + "^")
