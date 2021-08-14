package me.shadaj.scalapy.py

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.quoted.*
import scala.language.dynamics

class native extends StaticAnnotation
import scala.reflect.TypeTest

object FacadeImpl {
  
  def calleeParamRefs(using Quotes)(callee: quotes.reflect.Symbol): List[List[quotes.reflect.Term]] = {
    import quotes.reflect.*
    
    val termParamss = callee.paramSymss.filterNot(_.headOption.exists(_.isType))
    termParamss.map(_.map(x => Ref.apply(x)))
  }

  def creator = ???

  def native_impl[T: Type](using Quotes): Expr[T] = {
    import quotes.reflect.*

    def searchImplicit(typeReprParameter: TypeRepr): Term = {
      Implicits.search(typeReprParameter) match {
        case success: ImplicitSearchSuccess => {
          success.tree
        }
        case _ => {
          '{}.asTerm
        }
      }
    }

    val classDynamicSymbol = Symbol.requiredClass("me.shadaj.scalapy.py.Dynamic")
    val classReaderSymbol = Symbol.requiredClass("me.shadaj.scalapy.readwrite.Reader")
    val classWriterSymbol = Symbol.requiredClass("me.shadaj.scalapy.readwrite.Writer")
    val methodFromSymbol = Symbol.requiredMethod("me.shadaj.scalapy.py.Any.from")
    val classAnySymbol = Symbol.requiredClass("me.shadaj.scalapy.py.Any")

    val anyTypeTree = TypeIdent(classAnySymbol)

    val readerTypeRepr = TypeIdent(classReaderSymbol).tpe
    val writerTypeRepr = TypeIdent(classWriterSymbol).tpe
    val dynamicTypeRepr = TypeIdent(classDynamicSymbol).tpe

    val applyDynamicTypeToReaderType = readerTypeRepr.appliedTo(dynamicTypeRepr)
    val applyTTypeToReaderType = readerTypeRepr.appliedTo(TypeTree.of[T].tpe)

    val evidenceForDynamic = searchImplicit(applyDynamicTypeToReaderType)
    val evidenceForTypeT = searchImplicit(applyTTypeToReaderType)

    val callee = Symbol.spliceOwner.owner
    val methodName = callee.name
    val refss = calleeParamRefs(callee)
    if refss.length > 1 then
      report.throwError(s"callee $callee has curried parameter lists.")
    val args = refss.headOption.toList.flatten

    if (args.isEmpty) {
      val selectDynamicTerm = Apply(TypeApply(Select.unique(Apply(Select.unique(Apply(TypeApply(
        Select.unique(resolveThis,"as"), List(TypeIdent(classDynamicSymbol))), List(evidenceForDynamic)),  
        "selectDynamic"), List(Expr(methodName).asTerm)), "as"), List(TypeTree.of[T])), List(evidenceForTypeT)) 
      selectDynamicTerm.asExprOf[T]
    }
    else {
      def constructMethodFromTerm(arg: quotes.reflect.Term) = {
        val argumentType = arg.tpe.typeSymbol
        val applyArgTypeToWriter = writerTypeRepr.appliedTo(TypeIdent(argumentType).tpe)
        val tree = Apply(Apply(TypeApply(Ref(methodFromSymbol),List(TypeIdent(argumentType))),List(arg)),
          List(searchImplicit(applyArgTypeToWriter)))
        tree
      }

      val typedVarargs = Typed(Inlined(None, Nil, Repeated(args.map(x => constructMethodFromTerm(x)),
        anyTypeTree)), Applied(TypeIdent(defn.RepeatedParamClass), List(anyTypeTree)))
      val applyDynamicTerm = Apply(TypeApply(Select.unique(Apply(Apply(Select.unique(Apply(TypeApply(
        Select.unique(resolveThis,"as"), List(TypeIdent(classDynamicSymbol))), List(evidenceForDynamic)),
        "applyDynamic"), List(Expr(methodName).asTerm)), List(typedVarargs)), "as"), List(TypeTree.of[T])),
        List(evidenceForTypeT))
      applyDynamicTerm.asExprOf[T]
    }
  }

  def resolveThis(using Quotes): quotes.reflect.Term = {
    import quotes.reflect.*
    var sym = Symbol.spliceOwner
    while sym != null && !sym.isClassDef do
      sym = sym.owner
    This(sym)
  }

  def native_named_impl[T: Type](using Quotes): Expr[T] = {
    import quotes.reflect.*

    def searchImplicit(typeReprParameter: TypeRepr): Term = {
      Implicits.search(typeReprParameter) match {
        case success: ImplicitSearchSuccess => {
          success.tree
        }
        case _ => {
          '{}.asTerm
        }
      }
    }

    val classDynamicSymbol = Symbol.requiredClass("me.shadaj.scalapy.py.Dynamic")
    val classReaderSymbol = Symbol.requiredClass("me.shadaj.scalapy.readwrite.Reader")
    val classWriterSymbol = Symbol.requiredClass("me.shadaj.scalapy.readwrite.Writer")
    val methodFromSymbol = Symbol.requiredMethod("me.shadaj.scalapy.py.Any.from")
    val classAnySymbol = Symbol.requiredClass("me.shadaj.scalapy.py.Any")

    val anyTypeTree = TypeIdent(classAnySymbol)

    val readerTypeRepr = TypeIdent(classReaderSymbol).tpe
    val writerTypeRepr = TypeIdent(classWriterSymbol).tpe
    val dynamicTypeRepr = TypeIdent(classDynamicSymbol).tpe

    val applyDynamicTypeToReaderType = readerTypeRepr.appliedTo(dynamicTypeRepr)
    val applyTTypeToReaderType = readerTypeRepr.appliedTo(TypeTree.of[T].tpe)

    val evidenceForDynamic = searchImplicit(applyDynamicTypeToReaderType)
    val evidenceForTypeT = searchImplicit(applyTTypeToReaderType)

    val callee = Symbol.spliceOwner.owner
    val methodName = callee.name
    val refss = calleeParamRefs(callee)
    if refss.length > 1 then
      report.throwError(s"callee $callee has curried parameter lists.")
    val args = refss.headOption.toList.flatten
    if (args.isEmpty) {
      val selectDynamicTerm = Apply(TypeApply(Select.unique(Apply(Select.unique(Apply(TypeApply(
        Select.unique(resolveThis,"as"), List(TypeIdent(classDynamicSymbol))), List(evidenceForDynamic)),  
        "selectDynamic"),List(Expr(methodName).asTerm)),"as"), List(TypeTree.of[T])), List(evidenceForTypeT)) 
      selectDynamicTerm.asExprOf[T]
    }
    else {
      def constructMethodFromTerm(arg: quotes.reflect.Term) = {
        val argumentType = arg.tpe.typeSymbol
        val applyArgTypeToWriter = writerTypeRepr.appliedTo(TypeIdent(argumentType).tpe)
        val tree = Apply(Apply(TypeApply(Ref(methodFromSymbol), List(TypeIdent(argumentType))), List(arg)),
          List(searchImplicit(applyArgTypeToWriter)))
        tree
      }
      def constructTupleTerm(arg1: quotes.reflect.Term, arg2: quotes.reflect.Term) = {
        val tree = Expr.ofTuple((arg1.asExprOf[String], arg2.asExpr)).asTerm
        tree
      }
      
      val tupleType = Inferred(TypeTree.of[Tuple2].tpe.appliedTo(List(TypeTree.of[String].tpe, anyTypeTree.tpe)))
      val tupleList = args.map(x => constructTupleTerm(Expr(x.show).asTerm, constructMethodFromTerm(x)))
      val typedVarargs = Typed(Inlined(None, Nil, Repeated(tupleList, tupleType)),
        Applied(TypeIdent(defn.RepeatedParamClass), List(tupleType)))

      val applyDynamicNamedTerm = Apply(TypeApply(Select.unique(Apply(Apply(Select.unique(Apply(TypeApply(
        Select.unique(resolveThis, "as"), List(TypeIdent(classDynamicSymbol))), List(evidenceForDynamic)),
        "applyDynamicNamed"), List(Expr(methodName).asTerm)), List(typedVarargs)), "as"), List(TypeTree.of[T])),
        List(evidenceForTypeT))
      applyDynamicNamedTerm.asExprOf[T]
    }
  }
}
