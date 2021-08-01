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
    termParamss.map(_.map(x => Ref.apply(x)))
  }

  // def creator[T <: Any](using tagType: quoted.Type[T])(using Quotes): Expr[T] = {
  //   '{new _root_.me.shadaj.scalapy.py.FacadeCreator[$tagType] {
  //     def create(value: _root_.me.shadaj.scalapy.interpreter.PyValue) = new _root_.me.shadaj.scalapy.py.FacadeValueProvider(value) with $tagType {}
  //   }}
  // }
  def creator = ???



  def native_impl[T: Type](using Quotes): Expr[T] = {
    import quotes.reflect.*
    def searchImplicit(typeReprParameter: TypeRepr): Term = {
      Implicits.search(typeReprParameter) match {
        case success: ImplicitSearchSuccess => {
          println("AST for some implicit:   " + success.tree)
          success.tree
        }
        case _ => {
          println("AST ERROR")
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
    val evidenceForDynamic = Implicits.search(applyDynamicTypeToReaderType) match {
      case success: ImplicitSearchSuccess => {
        println("AST:   " + success.tree)
        success.tree
      }
    }
    // extract to other method
    val evidenceForTypeT = Implicits.search(applyTTypeToReaderType) match {
        case success: ImplicitSearchSuccess => {
          println("AST:   " + success.tree)
          success.tree
      }
    }
    //classDynamicSymbol.declaredMethods.foreach(println)
    val callee = Symbol.spliceOwner.owner
    val methodName = callee.name
    val refss = calleeParamRefs(callee)
    println("Method name:  " + methodName)
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
      println("TYPE OF SELECTDYNAMIC TREE: " + selectDynamicTerm.tpe)
      println("EXPR: " + selectDynamicTerm)
      println(selectDynamicTerm.show)
      selectDynamicTerm.asExprOf[T]
    }
    else {
// StringObjectFacade.this.as[me.shadaj.scalapy.py.Dynamic]
// (me.shadaj.scalapy.readwrite.Reader.facadeReader[me.shadaj.scalapy.py.Dynamic](me.shadaj.scalapy.py.FacadeCreator.getCreator[me.shadaj.scalapy.py.Dynamic]))
// .applyDynamic("replace")
// (me.shadaj.scalapy.py.Any.from[scala.Predef.String](old)(me.shadaj.scalapy.readwrite.Writer.stringWriter),
//  me.shadaj.scalapy.py.Any.from[scala.Predef.String](newValue)(me.shadaj.scalapy.readwrite.Writer.stringWriter))
//  .as[java.lang.String](me.shadaj.scalapy.readwrite.Reader.stringReader)
      def implicitParameters(arg: quotes.reflect.Term) = {
        println("ARGUMENT TYPE " + arg.tpe)
        val argumentType = arg.tpe.typeSymbol
    //    println("ARGUMENT TYPE IDENT: " + TypeIdent(argumentType).tpe)
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
        //println("TREE FOR IMPLICIT: " + tree.show)
        //println("TYPE OF IMPLICIT TREE " + tree.tpe.show)
        tree
      }
            // println("METHOD 1: " +TypeApply(
            //       Select.unique(
            //       Varargs(args.map(x => implicitParameters(x)).map(x => {
            //           anyTypeRepr.tpe.asType match {
            //             case '[t] =>{
            //                 println("VALUEEE:")
            //                 x.asExprOf[t]
            //             } 
            //           }
            //     })).asTerm,
            //     "asInstanceOf"
            //   ),
            //   List(anyTypeRepr)
            //   ).tpe.show)
      println("VARARG TYPE: "+ Varargs(args.map(x => implicitParameters(x)).map(x => {
                      anyTypeRepr.tpe.asType match {
                        case '[t] =>{

                            x.asExprOf[t]
                        } 
                      }
                })).asTerm.tpe.show)
      // println("VARARG TREE: " + Varargs(args.map(x => implicitParameters(x)).map(_.asExpr)).asTerm.show)

      // println("ANY TYPE AS TYPE: " + anyTypeRepr.tpe.asType)
      // println("TYPE: " + Varargs(args.map(x => implicitParameters(x)).map(x => {
      //                 anyTypeRepr.tpe.asType match {
      //                   case '[t] =>{
      //                     //  println("VALUEEE:")
      //                       println("TYPE OF T: " + TypeTree.of[t].show)
      //                       x.asExprOf[t]
      //                   } 
      //                 }
      //         })).asTerm.tpe.show)
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
              // implicit conversion 
              // for each arg
              List(
                TypeApply(
                  Select.unique(
                  Varargs(args.map(x => implicitParameters(x)).map(x => {
                      anyTypeRepr.tpe.asType match {
                        case '[t] =>{
                            x.asExprOf[t]
                        } 
                      }
                })).asTerm,
                "asInstanceOf"
              ),
                List(anyTypeRepr)
              )
            )
                
            ),
            "as"
          ),
          List(TypeTree.of[T])         
        ),
        List(evidenceForTypeT)
       ) 
      
      // val applyDynamicType =             
      //   Apply(    // this.as[Dynamic](evidence).applyDynamic(methodName)(parameters)
      //         Apply(  // this.as[Dynamic](evidence).applyDynamic(methodName)
      //           Select.unique( // this.as[Dynamic](evidence).applyDynamic
      //             Apply(      // this.as[Dynamic](evidence)
      //               TypeApply(  // this.as[Dynamic]
      //                 Select.unique( // this.as
      //                   resolveThis,  // this
      //                   "as"
      //                 ),
      //                 List(TypeIdent(classDynamicSymbol))
      //               ),
      //               List(evidenceForDynamic)
      //             ),
      //             "applyDynamic"
      //           ),
      //           List(Expr(methodName).asTerm)
      //         ),
      //         // implicit conversion 
      //         // for each arg
      //         List(Varargs(args.map(x => implicitParameters(x)).map(_.asExpr)).asTerm)
      //       )

      
      //println("TYPE OF FINAL APPLY TREE: " + applyDynamicType.tpe.show)
      //println(applyDynamicTerm.show)
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
