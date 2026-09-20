package autoset.model

/** Where a reader is in a configuration, and where it reports problems.
  *
  * This is threaded through `Reader.read`, so that anything a reader needs
  * besides the value it reads can be added without changing every reader.
  *
  * @param path
  *   the path of the value being read, e.g. `Vector("db", "host")`. It is
  *   empty at the root of a configuration.
  * @param reporter
  *   where to report warnings and errors.
  */
case class Context(path: Vector[String], reporter: Reporter):

  /** The context of the value at `key` inside the one being read. */
  def /(key: String): Context = copy(path = path :+ key)

  /** The context of the value at `index` inside the list being read. */
  def /(index: Int): Context = copy(path = path :+ index.toString)

  /** The path as it is shown in messages, e.g. `db.host`, empty at the root. */
  def show: String = path.mkString(".")

object Context:
  /** The context of a whole configuration, reporting to `reporter`. */
  def apply(reporter: Reporter): Context = Context(Vector.empty, reporter)
