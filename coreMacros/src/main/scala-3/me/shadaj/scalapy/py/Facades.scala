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

  // def creator[T <: Any](using tagType: quoted.Type[T])(using Quotes): Expr[T] = {
  //   '{new _root_.me.shadaj.scalapy.py.FacadeCreator[$tagType] {
  //     def create(value: _root_.me.shadaj.scalapy.interpreter.PyValue) = new _root_.me.shadaj.scalapy.py.FacadeValueProvider(value) with $tagType {}
  //   }}
  // }
  // def creator = ???
  //def native_impl = ???
 // def creator = ???
  // sublime + autocomplete, ask on gitter about metals scala 3 support
  /*
      reflect level
      create separate project
      have a macro
      expression == quote
  */
  // 2 inline methods; unseal in Expr 
  // expr is quote -> write a quote 
  def native_impl[T: Type](using Quotes): Expr[T] = {
    import quotes.reflect.*
    //  val castSym = Symbol.requiredMethod("stuff.cast")
    val methodSelectDynamic = Symbol.requiredMethod("me.shadaj.scalapy.py.Dynamic.selectDynamic")
    val methodAs = Symbol.requiredMethod("me.shadaj.scalapy.py.Any.as")
    val classDynamicSymbol = Symbol.requiredClass("me.shadaj.scalapy.py.Dynamic")
    
    val callee = Symbol.spliceOwner.owner
    
    val refss = calleeParamRefs(callee)
    if refss.length > 1 then
      report.throwError(s"callee $callee has curried parameter lists.")
    val args = refss.headOption.toList.flatten
 // if args.isEmpty then {
  //  val str = TermRef('{callee.name}.asTerm.tpe, callee.name) 
    val tree = TypeApply(Select(TypeApply(Select(TypeApply(Ref(methodAs),List(TypeIdent(classDynamicSymbol))),
  methodSelectDynamic),List(TypeIdent(callee))),methodAs),List(TypeTree.of[T]))
    //'{as[_root_.me.shadaj.scalapy.py.Dynamic].selectDynamic(${Expr(callee.name)}).as[T]}  
    tree.asExprOf[T]
  //}

  //else
    //'{as[_root_.me.shadaj.scalapy.py.Dynamic].applyDynamic($callee.name)(${Expr.ofSeq(args.map(_.asExpr))}*).as[T]}
  }

  def native_named_impl = ???
}
