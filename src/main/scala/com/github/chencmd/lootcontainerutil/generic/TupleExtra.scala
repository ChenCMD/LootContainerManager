package com.github.chencmd.lootcontainermanager.generic

import scala.compiletime.constValue
import scala.compiletime.ops.int.*

object TupleExtra {
  import Tuple.*

  case class PrecomputedAddition[n <: Int, m <: Int](val value: (n + m)) extends AnyVal

  object PrecomputedAddition {
    inline given precomputedAddition[n <: Int, m <: Int]: PrecomputedAddition[n, m] = {
      PrecomputedAddition[n, m](constValue[n + m])
    }
  }

  type Replace[T <: NonEmptyTuple, N <: Int, A] = Concat[Take[T, N], A *: Drop[T, N + 1]]

  extension [X <: NonEmptyTuple](tuple: X) {
    def replace[A](n: Int, a: A)(using pa: PrecomputedAddition[n.type, 1]): Replace[X, n.type, A] = {
      tuple.take(n) ++ (a *: tuple.drop(pa.value: (n.type + 1)))
    }

    def modify[A <: AnyRef](n: Int)(f: Elem[X, n.type] => A)(using
      pa: PrecomputedAddition[n.type, 1]
    ): Replace[X, n.type, A] = replace(n, f(tuple(n)))

  }
}
