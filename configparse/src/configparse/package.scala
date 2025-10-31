package configparse

// Note: ideally we'd use only plain exports instead of a package object and
// inheritance. Because of a compiler bug however, exported mehods can lose
// their default parameters. Hence, until https://github.com/lampepfl/dotty/issues/17930
// we'll need to revert to using a package object.
// object `package` extends api.MainApi

export configparse.model.Arr
export configparse.model.Config
export configparse.model.Null
export configparse.model.Origin
export configparse.model.Path
export configparse.model.Str
export configparse.model.Value

object default extends derivation.Api:
  def scalaNameToConfigName(str: String): String =
    configparse.util.TextUtils.snakify(str)

export default.*
