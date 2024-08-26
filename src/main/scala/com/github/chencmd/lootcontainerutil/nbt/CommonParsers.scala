package com.github.chencmd.lootcontainermanager.nbt

import scala.util.parsing.combinator.RegexParsers

trait CommonParsers extends RegexParsers {
  protected def integer: Parser[Int] = "-?[0-9]".r ^^ (_.toInt)

  protected def unquotedString: Parser[String] = """[a-zA-Z0-9_\-.+]+""".r

  protected def quotedString(allowEmpty: Boolean = false): Parser[String] = {
    val f: ((=> Parser[String]) => Parser[List[String]]) = if (allowEmpty) rep else rep1
    for {
      quote <- "'" | "\""
      str   <- f(s"[^$quote\\\\]".r | s"\\$quote" | "\\\\")
      _     <- quote
    } yield str.mkString
  }
}
