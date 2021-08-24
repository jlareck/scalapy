package me.shadaj.scalapy.py

import scala.language.dynamics
import scala.reflect.ClassTag

import me.shadaj.scalapy.readwrite.FacadeReader
import me.shadaj.scalapy.interpreter.CPythonInterpreter

@native trait Module extends Dynamic

object Module {
  def apply(module: String): Module = {
    Any.populateWith(CPythonInterpreter.importModule(module)).as[Module]
  }

  def apply(module: String, subname: String): Any = {
    Any.populateWith(CPythonInterpreter.importModule(module))
      .as[Dynamic].selectDynamic(subname)
  }
}
