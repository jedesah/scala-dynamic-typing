package com.github.jedesah.typelevel.test

import org.specs2.mutable._
import shapeless.test.illTyped

import scala.reflect.macros.TypecheckException

class MainSpec extends Specification {

  """dyn"code"""" should {
    "fail at compile time if a normal compile error" in {
      illTyped { """runtimeTypecheck{ "5 ++ 6" }""" }
      ok
    }
    "fail at compile time if it is completely invalid syntax'" in {
      illTyped { """runtimeTypecheck{ "5..9&#" }""" }
      ok
    }
    "work fine otherwise" in {
      "when dealing with values from Predef" in {
        val a = runtimeTypecheck {"5 + 6"}
        a === 11
      }
      "not require fully qualifying values from local scope" in {
        case class Cat(name: String, age: Int)
        val myCat = Cat("Fluf", 10)
        val age = runtimeTypecheck{"myCat.age"}
        age === 10
      }
      "produces incorrect code" in {
        runtimeTypecheck{"produceIncorrectCode"} must throwA[TypecheckException]
      }
      "macro expansion aborts" in {
        runtimeTypecheck{"abortMacroExpansion"} must throwA[TypecheckException]
      }
    }
  }
} 
