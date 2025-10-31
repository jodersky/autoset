package configparse.derivation

import configparse.model.Value
import configparse.model.Path

trait ReaderApi:
  trait Reader[A]:
    def read(value: Value, path: Path): Result[A]

  object Reader
