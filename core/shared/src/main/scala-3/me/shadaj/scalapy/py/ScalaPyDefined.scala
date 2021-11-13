package me.shadaj.scalapy.py

trait ScalaPyDefined {
  def registerScalaPyDefined(t: Any, bases: Dynamic*): Unit = 
    t.`type` = register(t, bases: _*)
    
  inline def register(t: Any, bases: Dynamic*): Dynamic = ${ScalaPyDefinedImpl.register('t, 'bases)}
}