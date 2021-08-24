package me.shadaj.scalapy

import me.shadaj.scalapy.interpreter.CPythonInterpreter
import me.shadaj.scalapy.py.Any

trait PyNone {
  @py.native trait None extends Any
  val None = Any.populateWith(CPythonInterpreter.noneValue).as[None]
}
