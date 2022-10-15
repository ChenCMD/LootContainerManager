package com.github.chencmd.lootcontainerutil.nbtpath

import com.github.chencmd.lootcontainerutil.nbtpath.definition.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object NBTPathParser extends RegexParsers {

  import NBTTag.*

  override val whiteSpace: Regex = """\s*""".r

  override def skipWhitespace: Boolean = false

  def parse(input: String): Either[String, NBTPath] =
    parseAll(nbtPath, input) match {
      case Success(res, _) => Right(res)
      case Failure(mes, _) => Left(mes)
      case Error(mes, _)   => Left(mes)
    }

  private def nbtPath: Parser[NBTPath] = for {
    root <- rootNode
    nodes <- node.*
  } yield NBTPath(root, nodes)

  private def rootNode: Parser[NBTPathRootNode] =
    matchRootObjectNode | matchObjectNode | compoundChildNode

  private def matchRootObjectNode: Parser[NBTPathRootNode.MatchRootObject] =
    compound ^^ NBTPathRootNode.MatchRootObject.apply

  private def matchObjectNode: Parser[NBTPathRootNode.MatchObject] = for {
    name <- quotedString() | unquotedString
    pattern <- compound
  } yield NBTPathRootNode.MatchObject(name, pattern)

  private def compoundChildNode: Parser[NBTPathRootNode.CompoundChild] =
    (quotedString() | unquotedString) ^^ NBTPathRootNode.CompoundChild.apply

  private def node: Parser[NBTPathNode] =
    nonRootMatchObjectNode | allElementsNode | matchElementNode | indexedElementNode | nonRootCompoundChildNode

  private def nonRootMatchObjectNode: Parser[NBTPathNode.MatchObject] =
    "." ~> matchObjectNode ^^ (n => NBTPathNode.MatchObject(n.name, n.pattern))

  private def allElementsNode: Parser[NBTPathNode.AllElements] =
    ".".? ~> "[]" ^^ (_ => NBTPathNode.AllElements())

  private def matchElementNode: Parser[NBTPathNode.MatchElement] =
    ".".? ~> "[" ~> compound <~ "]" ^^ NBTPathNode.MatchElement.apply

  private def indexedElementNode: Parser[NBTPathNode.IndexedElement] =
    ".".? ~> "[" ~> int <~ "]" ^^ (n => NBTPathNode.IndexedElement(n.value))

  private def nonRootCompoundChildNode: Parser[NBTPathNode.CompoundChild] =
    "." ~> compoundChildNode ^^ (n => NBTPathNode.CompoundChild(n.name))

  private def compound: Parser[NBTTagCompound] = for {
    _ <- "{"
    keyAndValue <- repsep(whiteSpace ~> compoundPair <~ whiteSpace, ",")
    _ <- "}"
  } yield NBTTagCompound(keyAndValue.toMap)

  private def compoundPair: Parser[(String, NBTTag)] = for {
    key <- quotedString() | unquotedString
    _ <- whiteSpace <~ ":" <~ whiteSpace
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
    toListParser(string) ^^ (l => NBTTagStringList(l.map(_.value)))

  private def byteList: Parser[NBTTagByteList] =
    toListParser(byte, "B") ^^ (l => NBTTagByteList(l.map(_.value)))

  private def shortList: Parser[NBTTagShortList] =
    toListParser(short) ^^ (l => NBTTagShortList(l.map(_.value)))

  private def intList: Parser[NBTTagIntList] =
    toListParser(int, "I") ^^ (l => NBTTagIntList(l.map(_.value)))

  private def longList: Parser[NBTTagLongList] =
    toListParser(long, "L") ^^ (l => NBTTagLongList(l.map(_.value)))

  private def floatList: Parser[NBTTagFloatList] =
    toListParser(float) ^^ (l => NBTTagFloatList(l.map(_.value)))

  private def doubleList: Parser[NBTTagDoubleList] =
    toListParser(double) ^^ (l => NBTTagDoubleList(l.map(_.value)))

  private def compoundList: Parser[NBTTagCompoundList] =
    toListParser(compound) ^^ (l => NBTTagCompoundList(l))

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

  private def unquotedString: Parser[String] = """[a-zA-Z0-9_\-.+]+""".r

  private def quotedString(allowEmpty: Boolean = false): Parser[String] = {
    for {
      quote <- "'" | "\""
      str <- (
        if (allowEmpty) rep
        else rep1: ((=> Parser[String]) => Parser[List[String]])
      )((s"[^$quote\\\\]").r | s"\\$quote" | "\\\\")
      _ <- quote
    } yield str.mkString
  }
}
