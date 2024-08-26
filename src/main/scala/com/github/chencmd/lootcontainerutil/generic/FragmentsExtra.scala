package com.github.chencmd.lootcontainermanager.generic

import cats.Functor
import cats.Reducible
import cats.implicits.*

import doobie.*
import doobie.implicits.*

object FragmentsExtra {
  import Fragments.*

  /** Returns `(f IN ((?, ?, ..., ?), (?, ?, ..., ?), ...))` foreach the values in `fs`. */
  def in[F[_]: Reducible: Functor, A: Write](f: Fragment, fs: F[A]): Fragment =
    parentheses(f ++ fr"IN" ++ tupled(fs.map(tupled)))

  /** Returns `(?, ?, ..., ?)` for the values in `a` */
  def tupled[A: Write](a: A): Fragment = parentheses(values(a))

  /** Returns `(fs0, fs1, ...)`. */
  def tupled[F[_]: Reducible: Functor](fs: F[Fragment]): Fragment = parentheses(comma(fs))

}
