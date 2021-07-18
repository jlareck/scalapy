package me.shadaj.scalapy.py


import scala.language.experimental.macros

import scala.annotation.StaticAnnotation
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.*
import scala.language.dynamics
class native extends StaticAnnotation


object FacadeImpl {
  
  def calleeParamRefs(using Quotes)(callee: quotes.reflect.Symbol): List[List[quotes.reflect.Term]] = {
    import quotes.reflect.*
    val termParamss = callee.paramSymss.filterNot(_.headOption.exists(_.isType))
    termParamss.map(_.map(Ref.apply))
  }

  def creator = ???

  def native_impl[T: Type](using Quotes): Expr[T] = {
    import quotes.reflect.*

    val methodSelectDynamic = Symbol.requiredMethod("me.shadaj.scalapy.py.Dynamic.selectDynamic")
    val methodApplyDynamic = Symbol.requiredMethod("me.shadaj.scalapy.py.Dynamic.applyDynamic")
    val methodAs = Symbol.requiredMethod("me.shadaj.scalapy.py.Any.as")
    val classDynamicSymbol = Symbol.requiredClass("me.shadaj.scalapy.py.Dynamic")    
    val callee = Symbol.spliceOwner.owner
    val refss = calleeParamRefs(callee)

    if refss.length > 1 then
      report.throwError(s"callee $callee has curried parameter lists.")
    val args = refss.headOption.toList.flatten
    val tree = TypeApply(Select(TypeApply(Select(TypeApply(Ref(methodAs),List(TypeIdent(classDynamicSymbol))),
                  methodSelectDynamic),List(TypeIdent(callee))),methodAs),List(TypeTree.of[T]))
    tree.asExprOf[T]
  }

  def native_named_impl = ???
}
