package com.github.jedesah

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object failTypeCheckAtRuntime {

  case class TypeCheckFailure(msg: String) extends Exception(msg)

  implicit class runtimeQuote(val ctx: StringContext) extends AnyVal {
    def dyn(args: Any*): Any = macro failTypeCheckAtRuntime
  }

  def failTypeCheckAtRuntime(c: Context)(args: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) =>
        if (rawParts.length != 1) c.abort(c.enclosingPosition, "Does not currently support actual String interpolation, funny heh?! :-)")
        val code = rawParts.head.asInstanceOf[Literal].value.value.asInstanceOf[String]
        val ast = c.parse(code)
        val actualCode = util.Try(c.typecheck(ast)).recover{ case t =>
          c.warning(c.enclosingPosition, t.getMessage)
          val msg = Literal(Constant(t.getMessage))
          q"""throw TypeCheckFailure($msg)"""
        }
        c.Expr(actualCode.get)
      case _ => throw new Exception("Improper use of this macro")
    }
  }
}

