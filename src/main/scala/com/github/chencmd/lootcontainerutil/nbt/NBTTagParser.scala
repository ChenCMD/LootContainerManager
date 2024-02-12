package com.github.chencmd.lootcontainerutil.nbt
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTNel
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag.*

import cats.data.NonEmptyList

import scala.util.matching.Regex

object NBTTagParser {
  def parse(input: String): Either[String, NBTTagCompound] = new NBTTagParser().parseNBTTag(input)
}

open class NBTTagParser extends CommonParsers {
  override val whiteSpace: Regex = """\s*""".r

  override def skipWhitespace: Boolean = false

  private def parseNBTTag(input: String): Either[String, NBTTagCompound] = {
    parseAll(compound, input) match {
      case Success(res, _) => Right(res)
      case Failure(err, _) => Left(err)
      case Error(err, _)   => Left(err)
    }
  }

  protected def compound: Parser[NBTTagCompound] = for {
    _           <- "{"
    keyAndValue <- repsep(whiteSpace ~> compoundPair <~ whiteSpace, ",")
    _           <- "}"
  } yield NBTTagCompound(keyAndValue.toMap)

  private def compoundPair: Parser[(String, NBTTag)] = for {
    key   <- quotedString() | unquotedString
    _     <- whiteSpace ~ ":" ~ whiteSpace
    value <- compound | byte | short | long | float | double | int | string | byteArray | intArray | longArray | list
  } yield (key, value)

  private def string: Parser[NBTTagString] = (quotedString(true) | unquotedString) ^^ NBTTagString.apply

  private def byte: Parser[NBTTagByte] =
    (("""-?\d+""".r <~ ("b" | "B")) | ("false" ^^ (_ => "0")) | ("true" ^^ (_ => "1"))) ^^ (s => NBTTagByte(s.toByte))

  private def short: Parser[NBTTagShort] = """-?\d+""".r <~ ("s" | "S") ^^ (s => NBTTagShort(s.toShort))

  private def int: Parser[NBTTagInt] = """-?\d+""".r ^^ (s => NBTTagInt(s.toInt))

  private def long: Parser[NBTTagLong] = """-?\d+""".r <~ ("l" | "L") ^^ (s => NBTTagLong(s.toLong))

  private def float: Parser[NBTTagFloat] = """-?(\d*\.)?\d+""".r <~ ("f" | "F") ^^ (s => NBTTagFloat(s.toFloat))

  private def double: Parser[NBTTagDouble] =
    (
      ("""-?(\d*\.)?\d+""".r <~ ("d" | "D"))
        | """-?\d*\.\d+""".r
    ) ^^ (s => NBTTagDouble(s.toDouble))

  private def byteArray: Parser[NBTTagByteArray] = toListParser(byte, "B") ^^ (l => NBTTagByteArray(l.toVector))

  private def intArray: Parser[NBTTagIntArray] = toListParser(int, "I") ^^ (l => NBTTagIntArray(l.toVector))

  private def longArray: Parser[NBTTagLongArray] = toListParser(long, "L") ^^ (l => NBTTagLongArray(l.toVector))

  private def list: Parser[NBTTagList] = {
    emptyList | compoundList | byteList | shortList | longList | floatList | intList | doubleList | stringList | nestedList
  }

  private def emptyList: Parser[NBTTagList] = "[]" ^^ (_ => NBTTag.listFrom())

  private def stringList: Parser[NBTTagList] = toListParser(string) ^^ (a => NBTTag.listFrom(NBTNel.String(a)))

  private def byteList: Parser[NBTTagList] = toListParser(byte) ^^ (a => NBTTag.listFrom(NBTNel.Byte(a)))

  private def shortList: Parser[NBTTagList] = toListParser(short) ^^ (a => NBTTag.listFrom(NBTNel.Short(a)))

  private def intList: Parser[NBTTagList] = toListParser(int) ^^ (a => NBTTag.listFrom(NBTNel.Int(a)))

  private def longList: Parser[NBTTagList] = toListParser(long) ^^ (a => NBTTag.listFrom(NBTNel.Long(a)))

  private def floatList: Parser[NBTTagList] = toListParser(float) ^^ (a => NBTTag.listFrom(NBTNel.Float(a)))

  private def doubleList: Parser[NBTTagList] = toListParser(double) ^^ (a => NBTTag.listFrom(NBTNel.Double(a)))

  private def compoundList: Parser[NBTTagList] = toListParser(compound) ^^ (a => NBTTag.listFrom(NBTNel.Compound(a)))

  private def nestedList: Parser[NBTTagList] = toListParser(list) ^^ (a => NBTTag.listFrom(NBTNel.List(a)))

  private def toListParser[A](elemParser: Parser[A], header: String): Parser[List[A]] =
    s"[$header;" ~> repsep(whiteSpace ~> elemParser <~ whiteSpace, ",") <~ "]"

  private def toListParser[A](elemParser: Parser[A]): Parser[NonEmptyList[A]] =
    "[" ~> rep1sep(whiteSpace ~> elemParser <~ whiteSpace, ",") <~ "]" ^^ NonEmptyList.fromListUnsafe
}
