package configparse.formats

import FileReader.Result
import java.io.InputStream
import org.ekrich.config
import configparse.model.{Config, Null, Str, Value, Arr, Origin}

object HoconReader extends FileReader:
  import scala.jdk.CollectionConverters.*

  def traverse(filename: String, value: config.ConfigValue): Value =
    val origin = Origin.File(filename, value.origin.lineNumber, -1)

    value.valueType match
      case config.ConfigValueType.OBJECT =>
        val cfg = Config()
        cfg.origins = List(origin)
        for (k, v) <- value.asInstanceOf[config.ConfigObject].asScala do
          cfg.fields(k) = traverse(filename, v)
        cfg
      case config.ConfigValueType.LIST =>
        val arr = Arr()
        arr.origins = List(origin)
        for elem <- value.asInstanceOf[config.ConfigList].asScala do
          arr.elems += traverse(filename, elem)
        arr
      case config.ConfigValueType.NULL =>
        val n = Null()
        n.origins = List(origin)
        n
      case _ =>
        val s = Str(value.unwrapped.toString)
        s.origins = List(origin)
        s

  override def read(name: String, stream: InputStream, sizeHint: Int): Result =
    val reader = java.io.InputStreamReader(stream)
    try
      val hocon = config.ConfigFactory.parseReader(reader)
      val cfg = traverse(name, hocon.root).asInstanceOf[Config]
      Result.Success(cfg)
    catch
      case ex: config.ConfigException =>
        Result.Error(ex.getMessage)
