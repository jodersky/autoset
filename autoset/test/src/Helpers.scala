package test

import autoset.model.*
import collection.mutable as m

object Helpers:
  def file(line: Int, path: String = "app.conf") = Origin.File(path, 0, line, 1)
  def env(name: String) = Origin.Env(name)
  def props(name: String) = Origin.Props(name)

  def str(raw: String, origins: Origin*) = Str(raw, LitKind.String, origins.toList)
  def nul(origins: Origin*) = Null(origins.toList)
  def arr(origins: Origin*)(values: Value*) = Arr(m.ListBuffer(values*), origins.toList)
  def obj(origins: Origin*)(fields: (String, Value)*) =
    Obj(m.LinkedHashMap(fields*), origins.toList)
