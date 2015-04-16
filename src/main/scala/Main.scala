package com.github.jedesah.typelevel

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.TypecheckException
import scala.reflect.macros.blackbox.Context

package object test {

  def runtimeTypecheck(identUnderTest: Any)(code: String) = macro failTypeCheckAtRuntime

  def runtimeTypecheck(code: String) = macro failMacroTypeCheckAtRuntime
  
  def produceIncorrectCode: Unit = macro produceIncorrectCodeImpl
  
  def produceIncorrectCodeImpl(c: Context): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"4 ++ 5")
  }

  def abortMacroExpansion: Unit = macro abortMacroExpansionImpl

  def abortMacroExpansionImpl(c: Context): c.Expr[Unit] = {
    c.abort(c.enclosingPosition, "Doomed to fail!")
  }

  def failMacroTypeCheckAtRuntime(c: Context)(code: c.Expr[String]): c.Expr[Any] = {
    import c.universe._
    val codeString = code.tree.asInstanceOf[Literal].value.value.asInstanceOf[String]
    val ast = c.parse(codeString)
    val typeCheckResult = util.Try(c.typecheck(ast, withMacrosDisabled = true))
    if (typeCheckResult.isFailure) throw typeCheckResult.failed.get
    val actualCode = util.Try(c.typecheck(ast)).recover{ case t: TypecheckException =>
      new TypecheckException(NoPosition, t.msg)
      val msg = Literal(Constant(t.getMessage))
      c.warning(c.enclosingPosition, t.getMessage)
      q"""throw TypecheckException(null,$msg)"""
    }
    c.Expr(actualCode.get)
  }

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

