package test
package ini

import utest.*

object NegTest extends DynamicTestSuite {
  import configparse.ini

  testAll(Util.pwd / "ini" / "neg", _.ext == "ini"){ inFile =>
    val outFile = inFile / os.up / (inFile.baseName + ".txt")

    val s = os.read.inputStream(inFile)
    try {
      val parser = ini.Parser(s, ini.NoopVisitor)
      val err = assertThrows[ini.ParseException] {
        parser.parse()
      }
      DiffTools.assertNoDiff(outFile, err.pretty())
    } finally {
      s.close()
    }
  }

}
