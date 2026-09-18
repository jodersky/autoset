package autoset.model

/** A config value given on the command line.
  *
  * This is independent of how command line arguments are parsed: translate
  * the arguments which were given (but not defaults of the command line
  * parser, which would override other config) into `Arg`s.
  *
  * @param name
  *   the argument as the user wrote it, e.g. `--db-port`, used in messages
  * @param path
  *   the setting it sets, e.g. `List("db", "port")`
  * @param value
  *   the value, as given on the command line
  */
case class Arg(name: String, path: List[String], value: String)
