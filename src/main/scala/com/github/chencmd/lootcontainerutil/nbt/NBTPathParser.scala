package com.github.chencmd.lootcontainerutil.nbt

import com.github.chencmd.lootcontainerutil.nbt.definition.*

import scala.util.matching.Regex

object NBTPathParser {
  def parse(input: String): Either[String, NBTPath] = new NBTPathParser().parseNBTPath(input)
}

class NBTPathParser extends NBTTagParser {

  override val whiteSpace: Regex = """\s*""".r

  override def skipWhitespace: Boolean = false

  private def parseNBTPath(input: String): Either[String, NBTPath] = parseAll(nbtPath, input) match {
    case Success(res, _) => Right(res)
    case Failure(mes, _) => Left(mes)
    case Error(mes, _)   => Left(mes)
  }

  private def nbtPath: Parser[NBTPath] = for {
    root  <- rootNode
    nodes <- node.*
  } yield NBTPath(root, nodes)

  private def rootNode: Parser[NBTPathRootNode] = matchRootObjectNode | matchObjectNode | compoundChildNode

  private def matchRootObjectNode: Parser[NBTPathRootNode.MatchRootObject] =
    compound ^^ NBTPathRootNode.MatchRootObject.apply

  private def matchObjectNode: Parser[NBTPathRootNode.MatchObject] = for {
    name    <- quotedString() | unquotedString
    pattern <- compound
  } yield NBTPathRootNode.MatchObject(name, pattern)

  private def compoundChildNode: Parser[NBTPathRootNode.CompoundChild] =
    (quotedString() | unquotedString) ^^ NBTPathRootNode.CompoundChild.apply

  private def node: Parser[NBTPathNode] =
    nonRootMatchObjectNode | allElementsNode | matchElementNode | indexedElementNode | nonRootCompoundChildNode

  private def nonRootMatchObjectNode: Parser[NBTPathNode.MatchObject] =
    "." ~> matchObjectNode ^^ (n => NBTPathNode.MatchObject(n.name, n.pattern))

  private def allElementsNode: Parser[NBTPathNode.AllElements] = ".".? ~> "[]" ^^ (_ => NBTPathNode.AllElements())

  private def matchElementNode: Parser[NBTPathNode.MatchElement] =
    ".".? ~> "[" ~> compound <~ "]" ^^ NBTPathNode.MatchElement.apply

  private def indexedElementNode: Parser[NBTPathNode.IndexedElement] =
    ".".? ~> "[" ~> integer <~ "]" ^^ NBTPathNode.IndexedElement.apply

  private def nonRootCompoundChildNode: Parser[NBTPathNode.CompoundChild] =
    "." ~> compoundChildNode ^^ (n => NBTPathNode.CompoundChild(n.name))
}
