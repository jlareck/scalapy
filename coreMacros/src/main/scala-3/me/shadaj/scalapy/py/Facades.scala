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

    val anyTypeRepr = TypeIdent(classAnySymbol)
    val readerTypeRepr = TypeIdent(classReaderSymbol).tpe
    val writerTypeRepr = TypeIdent(classWriterSymbol).tpe
    val dynamicTypeRepr = TypeIdent(classDynamicSymbol).tpe
    val applyDynamicTypeToReaderType = readerTypeRepr.appliedTo(dynamicTypeRepr)
    val applyTTypeToReaderType = readerTypeRepr.appliedTo(TypeTree.of[T].tpe)
    val sequenceType = Inferred(TypeTree.of[Seq].tpe.appliedTo(TypeIdent(classAnySymbol).tpe))

    //println("SEQUENCE TYPE:   " + sequenceType.show)
    val evidenceForDynamic = searchImplicit(applyDynamicTypeToReaderType)
    // extract to other method
    val evidenceForTypeT = searchImplicit(applyTTypeToReaderType)

    val callee = Symbol.spliceOwner.owner
    val methodName = callee.name
    val refss = calleeParamRefs(callee)
    if refss.length > 1 then
      report.throwError(s"callee $callee has curried parameter lists.")
    val args = refss.headOption.toList.flatten
    
    if (args.isEmpty) {
      val selectDynamicTerm = 
       Apply(//this.as[Dynamic](evidence).selectDynamic(methodName).as[T](evidenceForT)
        TypeApply( //this.as[Dynamic](evidence).selectDynamic(methodName).as[T]
          Select.unique( // this.as[Dynamic](evidence).selectDynamic(methodName).as
            Apply( // this.as[Dynamic](evidence).selectDynamic(methodName)
              Select.unique( // this.as[Dynamic](evidence).selectDynamic
                Apply(      // this.as[Dynamic](evidence)
                  TypeApply(  // this.as[Dynamic]
                    Select.unique( // this.as
                      resolveThis,  // this
                      "as"
                    ),
                    List(TypeIdent(classDynamicSymbol))
                  ),
                  List(evidenceForDynamic)
                ),  
                "selectDynamic"
              ),
              //List(Literal(StringConstant(methodName)))
              List(Expr(methodName).asTerm)
            ),
            "as"
          ),
          List(TypeTree.of[T]) 
        ),
        List(evidenceForTypeT)
       ) 

      println(selectDynamicTerm.show)
      selectDynamicTerm.asExprOf[T]
    }
    else {
      def implicitParameters(arg: quotes.reflect.Term) = {
        val argumentType = arg.tpe.typeSymbol
        val applyArgTypeToWriter = writerTypeRepr.appliedTo(TypeIdent(argumentType).tpe)
        val tree = Apply(
          Apply(
            TypeApply(
              Ref(methodFromSymbol),
              List(TypeIdent(argumentType))
            ),
            List(arg)
          ),
          List(searchImplicit(applyArgTypeToWriter))
        )
        tree
      }

      val typedVarargs = Typed(Inlined(None, Nil, Repeated(args.map(x => implicitParameters(x)),  anyTypeRepr)), Applied(TypeIdent(defn.RepeatedParamClass), List(anyTypeRepr)))
      
      val applyDynamicTerm = 
       Apply(//  this.as[Dynamic](evidence).applyDynamic(methodName)(parameters).as[T](evidenceForT)
        TypeApply(   //  this.as[Dynamic](evidence).applyDynamic(methodName)(parameters).as[T]
          Select.unique(  //  this.as[Dynamic](evidence).applyDynamic(methodName)(parameters).as
            Apply(    // this.as[Dynamic](evidence).applyDynamic(methodName)(parameters)
              Apply(  // this.as[Dynamic](evidence).applyDynamic(methodName)
                Select.unique( // this.as[Dynamic](evidence).applyDynamic
                  Apply(      // this.as[Dynamic](evidence)
                    TypeApply(  // this.as[Dynamic]
                      Select.unique( // this.as
                        resolveThis,  // this
                        "as"
                      ),
                      List(TypeIdent(classDynamicSymbol))
                    ),
                    List(evidenceForDynamic)
                  ),
                  "applyDynamic"
                ),
                List(Expr(methodName).asTerm)
              ),
             List(typedVarargs)
            ),
            "as"
          ),
          List(TypeTree.of[T])         
        ),
        List(evidenceForTypeT)
       ) 
      println("METHOD: " + applyDynamicTerm.show)
      applyDynamicTerm.asExprOf[T]
    }
  }

  def resolveThis(using Quotes): quotes.reflect.Term =
    import quotes.reflect.*
    var sym = Symbol.spliceOwner  // symbol of method where the macro is expanded
    while sym != null && !sym.isClassDef do
      sym = sym.owner  // owner of a symbol is what encloses it: e.g. enclosing method or enclosing class
    This(sym)

  def native_named_impl = ???
}
