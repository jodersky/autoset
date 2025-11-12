package test

import utest._

object ValuesTest extends DynamicTestSuite {

  testAll(Util.pwd / "values"){ inFile =>
    val outFile = inFile / os.up / (inFile.baseName + ".out")

    val cfg = configparse.readConfig(Seq(inFile.relativeTo(os.pwd)))

    val b = StringBuilder()
    for (key, value) <- cfg.flatten().toSeq.sortBy(_._1) do
      b ++= key
      b ++= "="
      value match
        case configparse.Str(s) =>
          b ++= "'"
          b ++= s
          b ++= "'"
      b ++= "\n"

    DiffTools.assertNoDiff(outFile, b.result())
  }

}
