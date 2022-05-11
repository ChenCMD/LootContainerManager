package com.github.chencmd.lootcontainerutil.nbtpath

import com.github.chencmd.lootcontainerutil.nbtpath.definition.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object NBTPathParser extends RegexParsers {

  override def skipWhitespace: Boolean = false


  private def unquotedString: Parser[String] = """[^".\[{]""".r

  private def quotedString: Parser[String] = "\"" ~> unquotedString <~ "\""

  private def byte: Parser[CompoundValue.VByte] = for {
    value <- ("""-?\d+""".r <~ "b") |
      ("false" ^^ (_ => "0")) |
      ("true" ^^ (_ => "1"))
  } yield CompoundValue.VByte(value.toByte)

  private def short: Parser[CompoundValue.VShort] = for {
    value <- """-?\d""".r <~ "s" ^^ (_.toShort)
  } yield CompoundValue.VShort(value)

  private def int: Parser[CompoundValue.VInt] = for {
    value <- """-?\d+""".r ^^ (_.toInt)
  } yield CompoundValue.VInt(value)

  private def long: Parser[CompoundValue.VLong] = for {
    value <- """-?\d+""".r <~ "l" ^^ (_.toLong)
  } yield CompoundValue.VLong(value)

  private def float: Parser[CompoundValue.VFloat] = for {
    value <- """-?(\d+\.)?\d+""".r <~ "f" ^^ (_.toFloat)
  } yield CompoundValue.VFloat(value)

  private def double: Parser[CompoundValue.VDouble] = for {
    value <- """-?(\d+\.)?\d+""".r <~ opt("d") ^^ (_.toDouble)
  } yield CompoundValue.VDouble(value)

  private def toListParser[A](elemParser: Parser[A], header: Option[String]): Parser[List[A]] =
    "[" ~> opt(header ~> ";") ~> whiteSpace ~> repsep(elemParser, whiteSpace <~ "," <~ whiteSpace) <~ whiteSpace <~ "]"


  private def compoundPair: Parser[CompoundPair] = for {
    key <- quotedString | unquotedString
    _ <- whiteSpace <~ ":" <~ whiteSpace
    value <- quotedString | unquotedString | byte | short | long | float | double | int | compound
      | toListParser(quotedString) | toListParser(unquotedString) | toListParser(byte, "B")
      | toListParser(short) | toListParser(long, "L") | toListParser(float)
      | toListParser(double) | toListParser(int, "I") | toListParser(compound)
  } yield (key, value)

  private def compound: Parser[CompoundTag] = for {
    pairs <- "{" ~> whiteSpace ~> repsep(compoundPair, whiteSpace <~ "," <~ whiteSpace) <~ whiteSpace <~ "}"
  } yield CompoundTag(pairs.toMap)


  private def matchObjectNode: Parser[NBTPathNode.MatchObject] = ???

  private def allElementsNode: Parser[NBTPathNode.AllElements] = "[]".map(_ => NBTPathNode.AllElements)

  private def matchElementNode: Parser[NBTPathNode.MatchElement] = ???

  private def indexedElementNode: Parser[NBTPathNode.IndexedElement] = ???

  private def compoundChildNode: Parser[NBTPathNode.CompoundChild] = "."

  private def node: Parser[NBTPathNode] = allElementsNode

  private def rootNode: Parser[MatchRootObjectNode] = compound.map(MatchRootObjectNode.apply)

  private def nbtPath: Parser[NBTPath] = for {
    root <- opt(rootNode)
    nodes <- repsep(node, ".")
  } yield NBTPath(root, nodes)

  def parse(input: String): Either[String, NBTPath] = parseAll(nbtPath, input) match {
    case Success(res, _) => Right(res)
    case Failure(mes, _) => Left(mes)
    case Error(mes, _) => Left(mes)
  }
}
