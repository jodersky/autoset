package autoset.model

enum Severity:
  case Warning, Error

  def label: String = this match
    case Warning => "warning"
    case Error => "error"

/** A problem found while loading or reading config.
  *
  * @param origin
  *   where the problem is, if known
  * @param line
  *   the source line at `origin`, if known, shown with a caret under the
  *   column
  */
case class Diagnostic(
    severity: Severity,
    message: String,
    origin: Option[Origin] = None,
    line: String = ""
):

  /** The column of `origin`, or -1 if unknown. */
  def col: Int = origin match
    case Some(Origin.File(_, _, _, col, _)) => col
    case Some(Origin.Code(_, _, _, col)) => col
    case _ => -1

  /** Render this diagnostic as text, ending in a newline. */
  def render: String =
    val sb = StringBuilder()
    sb ++= severity.label ++= ": "
    origin.foreach(o => sb ++= o.pretty ++= ": ")
    sb ++= message += '\n'
    if line.nonEmpty then
      sb ++= line += '\n'
      if col > 0 then sb ++= " " * (col - 1) += '^' += '\n'
    sb.result()

/** Collects warnings and errors as diagnostics.
  *
  * Diagnostics are only recorded; rendering them is up to the caller, e.g.
  * with `print` once loading is done. For a reporter which also prints each
  * diagnostic as soon as it is reported, see `Reporter.printing`.
  *
  * @param onReport
  *   called with each diagnostic as it is reported
  */
class Reporter(onReport: Diagnostic => Unit = _ => ()):
  private val reported = collection.mutable.ListBuffer.empty[Diagnostic]
  private var _warnings: Int = 0
  private var _errors: Int = 0

  /** All diagnostics, in the order they were reported. */
  def diagnostics: List[Diagnostic] = reported.toList

  def warnings: Int = _warnings
  def errors: Int = _errors
  def hasErrors: Boolean = _errors > 0

  def report(diagnostic: Diagnostic): Unit =
    reported += diagnostic
    diagnostic.severity match
      case Severity.Warning => _warnings += 1
      case Severity.Error => _errors += 1
    onReport(diagnostic)

  def warn(message: String): Unit =
    report(Diagnostic(Severity.Warning, message))

  /** @param line the source line at `pos`, shown with a caret under the column */
  def warn(message: String, pos: Origin, line: String = ""): Unit =
    report(Diagnostic(Severity.Warning, message, Some(pos), line))

  def error(message: String): Unit =
    report(Diagnostic(Severity.Error, message))

  /** @param line the source line at `pos`, shown with a caret under the column */
  def error(message: String, pos: Origin, line: String = ""): Unit =
    report(Diagnostic(Severity.Error, message, Some(pos), line))

  /** All diagnostics rendered as text, in the order they were reported. */
  def render: String = reported.map(_.render).mkString

  /** Print all diagnostics to `stream`, in the order they were reported. */
  def print(stream: java.io.PrintStream = System.err): Unit = stream.print(render)

object Reporter:

  /** A reporter which prints each diagnostic to `stream` as soon as it is
    * reported, besides recording it.
    */
  def printing(stream: java.io.PrintStream = System.err): Reporter =
    Reporter(d => stream.print(d.render))
