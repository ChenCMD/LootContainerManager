package com.github.chencmd.lootcontainerutil.nbtpath

import com.github.chencmd.lootcontainerutil.nbtpath.definition.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object NBTPathParser extends RegexParsers {

  override val whiteSpace: Regex = """\s*""".r

  override def skipWhitespace: Boolean = false

  def parse(input: String): Either[String, NBTPath] = parseAll(nbtPath, input) match {
    case Success(res, _) => Right(res)
    case Failure(mes, _) => Left(mes)
    case Error(mes, _) => Left(mes)
  }


  private def nbtPath: Parser[NBTPath] = for {
    root <- rootNode
    nodes <- node.*
  } yield NBTPath(root, nodes)


  private def rootNode: Parser[NBTPathRootNode] =
    matchRootObjectNode | matchObjectNode | compoundChildNode

  private def matchRootObjectNode: Parser[NBTPathRootNode.MatchRootObject] =
    compoundTag ^^ NBTPathRootNode.MatchRootObject.apply

  private def matchObjectNode: Parser[NBTPathRootNode.MatchObject] = for {
    name <- quotedString | unquotedString
    pattern <- compoundTag
  } yield NBTPathRootNode.MatchObject(name, pattern)

  private def compoundChildNode: Parser[NBTPathRootNode.CompoundChild] =
    (quotedString | unquotedString) ^^ NBTPathRootNode.CompoundChild.apply


  private def node: Parser[NBTPathNode] =
    nonRootMatchObjectNode | allElementsNode | matchElementNode | indexedElementNode | nonRootCompoundChildNode

  private def nonRootMatchObjectNode: Parser[NBTPathNode.MatchObject] =
    "." ~> matchObjectNode ^^ (n => NBTPathNode.MatchObject(n.name, n.pattern))

  private def allElementsNode: Parser[NBTPathNode.AllElements] =
    ".".? ~> "[]" ^^ (_ => NBTPathNode.AllElements())

  private def matchElementNode: Parser[NBTPathNode.MatchElement] =
    ".".? ~> "[" ~> compoundTag <~ "]" ^^ NBTPathNode.MatchElement.apply

  private def indexedElementNode: Parser[NBTPathNode.IndexedElement] =
    ".".? ~> "[" ~> int <~ "]" ^^ (_.value) ^^ NBTPathNode.IndexedElement.apply

  private def nonRootCompoundChildNode: Parser[NBTPathNode.CompoundChild] =
    "." ~> compoundChildNode ^^ (n => NBTPathNode.CompoundChild(n.name))


  private def compoundTag: Parser[CompoundTag] =
    "{" ~> repsep(whiteSpace ~> compoundPair <~ whiteSpace, ",") <~ "}" ^^ (p => CompoundTag(p.toMap))

  private def compoundPair: Parser[CompoundPair] = for {
    key <- quotedString | unquotedString
    _ <- whiteSpace <~ ":" <~ whiteSpace
    value <- compound | byte | short | long | float | int | double | string
      | compoundList | byteList | shortList | longList | floatList | intList | doubleList | stringList
  } yield (key, value)


  private def string: Parser[CompoundValue.VString] =
    (quotedString | unquotedString) ^^ CompoundValue.VString.apply

  private def byte: Parser[CompoundValue.VByte] =
    (("""-?\d+""".r <~ ("b" | "B")) | ("false" ^^ (_ => "0")) | ("true" ^^ (_ => "1"))) ^^ (_.toByte) ^^ CompoundValue.VByte.apply

  private def short: Parser[CompoundValue.VShort] =
    """-?\d""".r <~ ("s" | "S") ^^ (_.toShort) ^^ CompoundValue.VShort.apply

  private def int: Parser[CompoundValue.VInt] =
    """-?\d+""".r ^^ (_.toInt) ^^ CompoundValue.VInt.apply

  private def long: Parser[CompoundValue.VLong] =
    """-?\d+""".r <~ ("l" | "L") ^^ (_.toLong) ^^ CompoundValue.VLong.apply

  private def float: Parser[CompoundValue.VFloat] =
    """-?(\d+\.)?\d+""".r <~ ("f" | "F") ^^ (_.toFloat) ^^ CompoundValue.VFloat.apply

  private def double: Parser[CompoundValue.VDouble] =
    """-?(\d+\.)?\d+""".r <~ ("d" | "D").? ^^ (_.toDouble) ^^ CompoundValue.VDouble.apply

  private def compound: Parser[CompoundValue.VCompound] =
    compoundTag ^^ CompoundValue.VCompound.apply

  private def stringList: Parser[CompoundValue.VStringList] =
    toListParser(string) ^^ (_.map(_.value)) ^^ CompoundValue.VStringList.apply

  private def byteList: Parser[CompoundValue.VByteList] =
    toListParser(byte, "B") ^^ (_.map(_.value)) ^^ CompoundValue.VByteList.apply

  private def shortList: Parser[CompoundValue.VShortList] =
    toListParser(short) ^^ (_.map(_.value)) ^^ CompoundValue.VShortList.apply

  private def intList: Parser[CompoundValue.VIntList] =
    toListParser(int, "I") ^^ (_.map(_.value)) ^^ CompoundValue.VIntList.apply

  private def longList: Parser[CompoundValue.VLongList] =
    toListParser(long, "L") ^^ (_.map(_.value)) ^^ CompoundValue.VLongList.apply

  private def floatList: Parser[CompoundValue.VFloatList] =
    toListParser(float) ^^ (_.map(_.value)) ^^ CompoundValue.VFloatList.apply

  private def doubleList: Parser[CompoundValue.VDoubleList] =
    toListParser(double) ^^ (_.map(_.value)) ^^ CompoundValue.VDoubleList.apply

  private def compoundList: Parser[CompoundValue.VCompoundList] =
    toListParser(compound) ^^ (_.map(_.value)) ^^ CompoundValue.VCompoundList.apply


  private def toListParser[A](elemParser: Parser[A], header: String = ""): Parser[List[A]] =
    "[" ~> (if header != "" then s"$header;" else "").? ~> repsep(whiteSpace ~> elemParser <~ whiteSpace, ",") <~ "]"


  private def unquotedString: Parser[String] = """[^".\[{]""".r

  private def quotedString: Parser[String] = "\"" ~> unquotedString <~ "\""
}
