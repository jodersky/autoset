package test

import utest._, framework._

trait DynamicTestSuite extends TestSuite {

  private val checks = IndexedSeq.newBuilder[(String, () => Unit)]

  protected def test(name: String)(fct: => Unit): Unit = {
    checks += (name -> (() => fct))
  }

  final override def tests: Tests = {
    val ts = checks.result()
    if (ts.isEmpty) {
      Tests.apply(())
    } else {
      val names = Tree("", ts.map(t => Tree(t._1))*)
      val thunks = new TestCallTree(Right(ts.map( t => new TestCallTree(Left(t._2())))))
      Tests.apply(names, thunks)
    }
  }

  protected def testAll(dir: os.Path, filter: os.Path => Boolean = _.ext != "out")(action: os.Path => Unit) = {
    for (inFile <- os.list(dir) if filter(inFile)) test(inFile.baseName + "_" + inFile.ext) {
      action(inFile)
    }
  }

}
