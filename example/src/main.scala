package example

// snippet example
case class Foo(
  name: String,
  value: Int = 42
) derives configparse.Reader

case class Settings(
  x: Int = 2,
  data: Map[String, Foo]
) derives configparse.Reader

def main(args: Array[String]): Unit =
  // read raw config (this cannot fail except for syntax errors in the config files)
  val config = configparse.readConfig(
    paths = Seq(os.sub / "example" / "src" / "config.yaml")
  )
  println("raw config result")
  println(config.dump())

  // map the config to a scala value or exit, showing any errors
  val settings = configparse.read[Settings](config = config)
  println("case class mapped result")
  println(settings)
//end snippet
