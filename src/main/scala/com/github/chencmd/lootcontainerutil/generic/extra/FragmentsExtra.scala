package com.github.chencmd.lootcontainermanager.generic.extra

import cats.Reducible
import cats.implicits.*

import doobie.*
import doobie.implicits.*

object FragmentsExtra {
  import Fragments.*

  /** Returns `(f IN ((?, ?, ..., ?), (?, ?, ..., ?), ...))` foreach the values in `fs`. */
  def in[F[_]: Reducible, A: Write](f: Fragment, fs: F[A]): Fragment = parentheses(f ++ fr"IN" ++ tupled(fs))

  /** Returns `(?, ?, ..., ?)` for the values in `a` */
  def tupled[A: Write](a: A): Fragment = parentheses(values(a))

  /** Returns `(tupled(fs0), tupled(fs1), ...)`. */
  def tupled[F[_]: Reducible, A: Write](fs: F[A]): Fragment = parentheses(comma(fs.toNonEmptyList.map(tupled)))
}
