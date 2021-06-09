package me.shadaj.scalapy.py

import scala.quoted.*
import scala.annotation.StaticAnnotation
class native extends StaticAnnotation

class FacadeCreator[T]
trait Any
class Bar[T <: Any](fImpl: T) extends FacadeCreator[T] { def create: T = fImpl }

object FacadeImpl {

  def creator[T <: Any](using Type[T], Quotes): Expr[FacadeCreator[T]] = 
    import quotes.reflect.*
    // new FacadeCreator[T] { def create: T = new T }
    // println(TypeTree.of[T])
    val bar = TypeIdent(Symbol.requiredClass("me.shadaj.scalapy.py.Bar"))
    Apply(TypeApply(Select.unique(New(bar),"<init>"),List(TypeTree.of[T])),List(Apply(Select.unique(New(TypeTree.of[T]),"<init>"),List()))).asExprOf[FacadeCreator[T]]

  def native_impl[T] = ???

  def native_named_impl[T] = ???
}
