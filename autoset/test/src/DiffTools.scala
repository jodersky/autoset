package test

object DiffTools {

  // Ignore output mistmatches and overwite expected output with the actual result. This can be
  // helpful after large-scale refactorings.
  // Use it with `OVERWRITE=yes`
  // Don't forget to inspect the resulting diff! Handle with care!
  def overwrite = sys.env.contains("OVERWRITE")

  def assertNoDiff(expected: os.Path, actual: String): Unit =
    if (overwrite) {
      os.write.over(expected, actual)
    } else {
      val (code, diff) = exec("diff", "-Z", "--context", expected.toString, os.temp(actual).toString)

      if (code != 0) {
        throw new java.lang.AssertionError(diff)
      }
    }

  def exec(commands: String*): (Int, String) = {
    val builder = new ProcessBuilder(commands*)
      //.redirectError(ProcessBuilder.Redirect.INHERIT)
      .redirectErrorStream(true)

    // builder.environment.clear()
    //for ((k, v) <- env) builder.environment.put(k, v)
    val process = builder.start()

    val reader = process.getInputStream()
    val out = new java.io.ByteArrayOutputStream
    try {
      val buffer = new Array[Byte](8192)
      var l = 0
      while
        l = reader.read(buffer)
        l != -1
      do
        out.write(buffer, 0, l)

      (process.waitFor(), out.toString())
    } finally {
      process.destroy()
      reader.close()
      process.waitFor()
    }
  }

}
