package com.github.jedesah

import scala.util.{Success, Try}

package object utils {

  implicit class AugmentedTry[A](underlying: Try[A]) {
    def failure: Option[Throwable] = underlying.transform(_ => Success(None), e => Success(Some(e))).get
  }
}
