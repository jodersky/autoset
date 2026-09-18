package autoset.model

/** A parser for a configuration file format, such as JSON or INI. */
trait FormatParser:

  /** Parse the content of a configuration file.
    *
    * Problems are reported to `reporter`. The result is `None` if and only if
    * at least one error was reported; a successful parse may still report
    * warnings.
    *
    * @param name
    *   The name of the config file. This is used as the path of the
    *   [[Origin.File]] origins of the parsed values.
    * @param stream
    *   Content stream of the file. The caller is responsible for closing it.
    * @param sizeHint
    *   Length of the content stream, or -1 if unknown. This should be used for
    *   optimization purposes only and may not correspond to the actual size
    *   (for example if the stream is not backed by an actual file).
    * @param reporter
    *   Where to report warnings and errors.
    */
  def parse(
      name: String,
      stream: java.io.InputStream,
      sizeHint: Int,
      reporter: Reporter
  ): Option[Obj]
