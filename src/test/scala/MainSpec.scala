package com.github.jedesah.typelevel.test

import org.specs2.matcher.TypecheckMatchers
import org.specs2.mutable._
import org.specs2.execute._, Typecheck._


import scala.reflect.macros.TypecheckException

class EvalSpec extends Specification with TypecheckMatchers {

  "all" should {
    "fail at compile time" >> {
      "if it's a normal compile error" in {
        "evalMacro" in {
          typecheck { """evalMacro{ "5 ++ 6" }"""} must not succeed
        }
        "evalTypelevel" in {
          typecheck { """evalTypelevel{ "5 ++ 6" }"""} must not succeed
        }
        "evalBoth" in {
          typecheck { """evalBoth{ "5 ++ 6" }"""} must not succeed
        }
      }
      "if it is completely invalid syntax'" in {
        "evalMacro" in {
          typecheck { """evalMacro{ "5..9&#" }"""} must not succeed
        }
        "evalTypelevel" in {
          typecheck { """evalTypelevel{ "5..9&#" }"""} must not succeed
        }
        "evalBoth" in {
          typecheck { """evalBoth{ "5..9&#" }"""} must not succeed
        }
      }
    }
    "compile and return the value at runtime if a perfectly normal expression" >> {
      "when dealing with values from Predef" in {
        "evalMacro" in {
          val a = evalMacro {
            "5 + 6"
          }
          a === 11
        }
        "evalTypelevel" in {
          val a = evalTypelevel {
            "5 + 6"
          }
          a === 11
        }
        "evalBoth" in {
          val a = evalBoth {
            "5 + 6"
          }
          a === 11
        }
      }
      "not require fully qualifying values from local scope" in {
        case class Cat(name: String, age: Int)
        val myCat = Cat("Fluf", 10)
        val age = evalMacro{"myCat.age"}
        age === 10
      }
    }
  }

  "evalMacro" should {
    "fail at compile time if it's a typecheck error due to a missing implicit" in {
      trait Evidence[A]
      def typelevelFun[A: Evidence](a: A) = a
      typecheck { """evalMacro{ "typelevelFun(5)" }"""} must not succeed
    }
    "fail at runtime" >> {
      "if a macro produces code that does not typecheck" in {
        evalMacro {"produceIncorrectCode"} must throwA[TypecheckException]
      }
      "if a macro aborts or otherwise throws an expection during expansion" in {
        evalMacro {"abortMacroExpansion"} must throwA[TypecheckException]
      }
    }
  }

  "evalTypelevel" should {
    "fail at compile time" >> {
      "if it's a normal compile error" in {
        typecheck { """evalTypelevel{ "5 ++ 6" }"""} must not succeed
      }
      "if it is completely invalid syntax'" in {
        typecheck { """evalTypelevel{ "5..9&#" }"""} must not succeed
      }
    }
    "fail at runtime if it's a typecheck error due to a missing implicit" in {
      trait Evidence[A]
      def typelevelFun[A: Evidence](a: A) = a
      evalTypelevel("typelevelFun(7)") must throwA[TypecheckException]
    }
  }

  "evalBoth" should {
    "fail at runtime" >> {
      "if a macro produces code that does not typecheck" in {
        evalBoth {"produceIncorrectCode"} must throwA[TypecheckException]
      }
      "if a macro aborts or otherwise throws an expection during expansion" in {
        evalBoth {"abortMacroExpansion"} must throwA[TypecheckException]
      }
      "if it's a typecheck error due to a missing implicit" in {
        trait Evidence[A]
        def typelevelFun[A: Evidence](a: A) = a
        evalBoth("typelevelFun(7)") must throwA[TypecheckException]
      }
    }
  }
} 
