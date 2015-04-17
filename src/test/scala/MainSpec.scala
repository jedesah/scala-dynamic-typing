package com.github.jedesah.typelevel.test

import org.specs2.matcher.TypecheckMatchers
import org.specs2.mutable._
import org.specs2.execute._, Typecheck._


import scala.reflect.macros.TypecheckException

class MainSpec extends Specification with TypecheckMatchers {

  """dyn"code"""" should {
    "fail at compile time if a normal compile error" in {
      typecheck { """runtimeTypecheck{ "5 ++ 6" }""" } must not succeed
    }
    "fail at compile time if it is completely invalid syntax'" in {
      typecheck { """runtimeTypecheck{ "5..9&#" }""" } must not succeed
    }
    "compile and return the value at runtime if a perfectly normal expression" >> {
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
    }
    "fail at runtime if a macro produces code that does not typecheck" in {
      runtimeTypecheck{"produceIncorrectCode"} must throwA[TypecheckException]
    }
    "fail at runtime if a macro aborts or otherwise throws an expection during expansion" in {
      runtimeTypecheck{"abortMacroExpansion"} must throwA[TypecheckException]
    }
  }
} 
