package com.github.jedesah
package typelevel

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.TypecheckException
import scala.reflect.macros.blackbox.Context
import utils.AugmentedTry

package object test {

  def evalMacro(identUnderTest: Any)(code: String) = macro failTypeCheckAtRuntime

  def evalMacro(code: String) = macro evalMacroImpl

  def evalTypelevel(code: String) = macro evalTypelevelImpl

  def evalBoth(code: String) = macro evalBothImpl
  
  def produceIncorrectCode: Unit = macro produceIncorrectCodeImpl
  
  def produceIncorrectCodeImpl(c: Context): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"4 ++ 5")
  }

  def abortMacroExpansion: Unit = macro abortMacroExpansionImpl

  def abortMacroExpansionImpl(c: Context): c.Expr[Unit] = {
    c.abort(c.enclosingPosition, "Doomed to fail!")
  }

  def evalMacroImpl(c: Context)(code: c.Expr[String]): c.Expr[Any] = {
    eval(deferMacro = true, deferImplicit = false)(c)(code)
  }

  def evalTypelevelImpl(c: Context)(code: c.Expr[String]): c.Expr[Any] = {
    eval(deferMacro = false, deferImplicit = true)(c)(code)
  }

  def evalBothImpl(c: Context)(code: c.Expr[String]): c.Expr[Any] = {
    eval(deferMacro = true, deferImplicit = true)(c)(code)
  }

  def eval(deferMacro: Boolean, deferImplicit: Boolean)(c: Context)(code: c.Expr[String]): c.Expr[Any] = {
    assume(deferMacro || deferImplicit)
    import c.universe._
    def warnAndFailAtRuntime(ex: TypecheckException) = {
      val msg = Literal(Constant(ex.getMessage))
      c.warning(c.enclosingPosition, ex.getMessage)
      q"""throw TypecheckException(null,$msg)"""
    }
    code.tree match {
      case Literal(Constant(codeString: String)) =>
        val ast = c.parse(codeString)
        // Figure out if the code fails without any macro expansion or missing implicits
        val noMacrosResult = util.Try(c.typecheck(ast, withMacrosDisabled = true))
        val newCode: Tree = noMacrosResult.failure.map {
          case error: TypecheckException if isImplicitMissingError(error) && deferImplicit => warnAndFailAtRuntime(error)
          case other: TypecheckException => throw other
        }.getOrElse {
          util.Try(c.typecheck(ast)).recover { case t: TypecheckException =>
            if (deferMacro) warnAndFailAtRuntime(t)
            else throw t
          }.get
        }
        c.Expr(newCode)
      case _ => c.abort(c.enclosingPosition, "This macro only works on String literals")
    }
  }

  // For now, a missing implicit error is any TypecheckException that starts with a certain phrasing
  def isImplicitMissingError(ex: TypecheckException) = ex.msg.startsWith("could not find implicit value")

  def failTypeCheckAtRuntime(c: Context)(identUnderTest: c.Expr[Any])(code: c.Expr[String]): c.Expr[Any] = {
    import c.universe._
    val codeString = code.tree.asInstanceOf[Literal].value.value.asInstanceOf[String]
    val ast = c.parse(codeString)
    object FindMacroLocations extends Traverser {
      val locations: mutable.ListBuffer[Position] = mutable.ListBuffer()
      override def traverse(tree: Tree): Unit = tree match {
        case ident@Ident(name) => {
          if (ident equalsStructure identUnderTest.tree) locations += ident.pos
        }
        case _ => super.traverse(tree)
      }
      def apply(tree: Tree) = {traverse(tree); locations.toList}
    }
    val locations = FindMacroLocations(ast)
    val actualCode = util.Try(c.typecheck(ast)).recover{ case t: TypecheckException =>
      val msg = Literal(Constant(t.getMessage))
      if (locations.exists(_.start == t.pos.start)) {
        c.warning(c.enclosingPosition, t.getMessage)
        q"""throw TypeCheckException(null, $msg)"""
      }
      else c.abort(c.enclosingPosition, t.getMessage)
    }
    c.Expr(actualCode.get)
  }
}

