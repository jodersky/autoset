package configparse

export configparse.model.Arr
export configparse.model.Config
export configparse.model.Origin
export configparse.model.Path
export configparse.model.Str
export configparse.model.Value

object default extends derivation.Api:
  def scalaNameToConfigName(str: String): String =
    configparse.util.TextUtils.snakify(str)

export default.*
