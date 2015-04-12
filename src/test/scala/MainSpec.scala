package com.github.jedesah.failTypeCheckAtRuntime

import org.specs2.mutable._
import shapeless.test.illTyped

class MainSpec extends Specification {

  """dyn"code"""" should {
    "fail at runtime if the string does not typecheck as code" in {
      dyn"5 ++ 6" should throwA[TypeCheckFailure]
    }
    "fail at compile time if it is completely invalid syntax'" in {
      illTyped { """dyn"5..9&#"""" }
      ok
    }
    "work fine otherwise" in {
      "when dealing with references from Predeg" in {
        val a = dyn"5 + 6"
        a === 11
      }
      "not require qualifying local references" in {
        case class Cat(name: String, age: Int)
        val myCat = Cat("Fluf", 10)
        val age = dyn"myCat.age"
        age === 10
      }
    }
  }
} 
