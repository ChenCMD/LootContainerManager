package com.github.chencmd.lootcontainerutil.nbt

import scala.util.parsing.combinator.RegexParsers

trait CommonParsers extends RegexParsers {
  protected def integer: Parser[Int] = "-?[0-9]".r ^^ (_.toInt)

  protected def unquotedString: Parser[String] = """[a-zA-Z0-9_\-.+]+""".r

  protected def quotedString(allowEmpty: Boolean = false): Parser[String] = {
    for {
      quote <- "'" | "\""
      str <- (
        if (allowEmpty) rep
        else rep1: ((=> Parser[String]) => Parser[List[String]])
        ) ((s"[^$quote\\\\]").r | s"\\$quote" | "\\\\")
      _ <- quote
    } yield str.mkString
  }
}
