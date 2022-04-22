package com.github.chencmd.protectlootchest.generic.extensions

import scala.reflect.TypeTest

object CastOps {
  extension[A] (value: A) {
    def downcastOrNone[B](using tt: TypeTest[A, B]): Option[B] =
      value match {
        case tt(b) => Some(b)
        case _ => None
      }
  }
}
