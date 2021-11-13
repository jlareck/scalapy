package me.shadaj.scalapy.py

import me.shadaj.scalapy.interpreter.PyValue

trait AnyRawValue {
  private[scalapy] var rawValue: PyValue = null
  private[scalapy] var `type`: Dynamic = null
  def initRawValue(params: Any*): Unit = 
    rawValue = `type`.apply(params: _*).rawValue
}

trait AnyPopulateWith {
  def populateWith(v: PyValue): Any = {
    new Any {
      rawValue = v
    }
  }
}
