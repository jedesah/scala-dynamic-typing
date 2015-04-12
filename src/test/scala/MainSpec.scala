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
      val a = dyn"5 + 6"
      a === 11
    }
  }
} 
