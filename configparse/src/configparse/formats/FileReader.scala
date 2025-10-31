package configparse.formats

import configparse.model.Config

trait FileReader:

  /** Read the content of a configuration file.
    * @param name The name of the config file.
    * @param stream Content stream of the file.
    * @param sizeHint Length of the content stream. This should be used for
    * optimization purposes only and may not actually correspond to the actual
    * size (for example if the stream is not backed by an actual file).
    */
  def read(
    name: String,
    stream: java.io.InputStream,
    sizeHint: Int
  ): FileReader.Result

object FileReader:
  enum Result:
    case Success(config: Config)
    case Error(
      message: String,
      row: Int = -1,
      col: Int = -1,
      line: String = ""
    )
