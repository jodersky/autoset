package test

object Util:
  val pwd = os.Path(sys.env("MILL_TEST_RESOURCE_DIR"))
