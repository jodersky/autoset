package test
package ini

import utest._

object CheckTest extends DynamicTestSuite {
  import configparse.ini


  class Visitor() extends ini.Visitor:
    import ini.Pos

    val root = ujson.Obj()
    var active = root
    var key: String = null

    def visitKey(pos: Pos, key: String) = this.key = key
    def visitString(pos: Pos, text: String) =
      active.value(key) = ujson.Str(text)
    def visitEmpty(pos: Pos): Unit = active.value(key) = ujson.Null
    def visitSection(pos: Pos, sectionKey: Seq[String]) =
      var obj = root
      for seg <- sectionKey do
        obj.value.get(seg) match
          case Some(o: ujson.Obj) =>
            obj = o
          case _ =>
            val o = ujson.Obj()
            obj.value(seg) = o
            obj = o
      active = obj

  testAll(Util.pwd / "ini" / "checks", _.ext == "ini"){ inFile =>
    val outFile = inFile / os.up / (inFile.baseName + ".json")

    val s = os.read.inputStream(inFile)
    try {
      val visitor = Visitor()
      val parser = ini.Parser(s, visitor)
      parser.parse()

      DiffTools.assertNoDiff(outFile, ujson.write(visitor.root, 2))
    } finally {
      s.close()
    }
  }

}
