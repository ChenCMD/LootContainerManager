package com.github.chencmd.lootcontainerutil.nbt

import definition.{NBTTag, NBTTagListType}

import scala.util.matching.Regex

import NBTTag.*

object NBTTagParser {
  def parse(input: String): Either[String, NBTTagCompound] = new NBTTagParser().parseNBTTag(input)
}

class NBTTagParser extends CommonParsers {
  override val whiteSpace: Regex = """\s*""".r

  override def skipWhitespace: Boolean = false

  private def parseNBTTag(input: String): Either[String, NBTTagCompound] = {
    parseAll(compound, input) match {
      case Success(res, _) => Right(res)
      case Failure(err, _) => Left(err)
      case Error(err, _) => Left(err)
    }
  }

  protected def compound: Parser[NBTTagCompound] = for {
    _ <- "{"
    keyAndValue <- repsep(whiteSpace ~> compoundPair <~ whiteSpace, ",")
    _ <- "}"
  } yield NBTTagCompound(keyAndValue.toMap)

  private def compoundPair: Parser[(String, NBTTag)] = for {
    key <- quotedString() | unquotedString
    _ <- whiteSpace ~ ":" ~ whiteSpace
    value <-
      compound | byte | short | long | float | int | double | string | list
  } yield (key, value)

  private def string: Parser[NBTTagString] =
    (quotedString(true) | unquotedString) ^^ NBTTagString.apply

  private def byte: Parser[NBTTagByte] =
    (("""-?\d+""".r <~ ("b" | "B")) | ("false" ^^ (_ => "0")) | ("true" ^^ (_ =>
      "1"))) ^^ (s => NBTTagByte(s.toByte))

  private def short: Parser[NBTTagShort] =
    """-?\d""".r <~ ("s" | "S") ^^ (s => NBTTagShort(s.toShort))

  private def int: Parser[NBTTagInt] =
    """-?\d+""".r ^^ (s => NBTTagInt(s.toInt))

  private def long: Parser[NBTTagLong] =
    """-?\d+""".r <~ ("l" | "L") ^^ (s => NBTTagLong(s.toLong))

  private def float: Parser[NBTTagFloat] =
    """-?(\d+\.)?\d+""".r <~ ("f" | "F") ^^ (s => NBTTagFloat(s.toFloat))

  private def double: Parser[NBTTagDouble] =
    """-?(\d+\.)?\d+""".r <~ ("d" | "D").? ^^ (s => NBTTagDouble(s.toDouble))

  private def list: Parser[NBTTagListType] = {
    compoundList | byteList | shortList | longList | floatList | intList | doubleList | stringList | nestedList
  }

  private def stringList: Parser[NBTTagStringList] =
    toListParser(string) ^^ NBTTagStringList.apply

  private def byteList: Parser[NBTTagByteList] =
    toListParser(byte, "B") ^^ NBTTagByteList.apply

  private def shortList: Parser[NBTTagShortList] =
    toListParser(short) ^^ NBTTagShortList.apply

  private def intList: Parser[NBTTagIntList] =
    toListParser(int, "I") ^^ NBTTagIntList.apply

  private def longList: Parser[NBTTagLongList] =
    toListParser(long, "L") ^^ NBTTagLongList.apply

  private def floatList: Parser[NBTTagFloatList] =
    toListParser(float) ^^ NBTTagFloatList.apply

  private def doubleList: Parser[NBTTagDoubleList] =
    toListParser(double) ^^ NBTTagDoubleList.apply

  private def compoundList: Parser[NBTTagCompoundList] =
    toListParser(compound) ^^ NBTTagCompoundList.apply

  private def nestedList: Parser[NBTTagNestedList] =
    toListParser(list) ^^ NBTTagNestedList.apply

  private def toListParser[A](
      elemParser: Parser[A],
      header: String = ""
  ): Parser[List[A]] = for {
    _ <- "["
    _ <- opt(if header != "" then s"$header;" else "")
    list <- repsep(whiteSpace ~> elemParser <~ whiteSpace, ",")
    _ <- "]"
  } yield list

}
