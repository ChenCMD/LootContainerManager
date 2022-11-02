package nbt.definition

import org.scalatest.funspec.AnyFunSpec

import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag

import cats.implicits.toTraverseOps

class NBTTagTest extends AnyFunSpec {
  import NBTTag.*
  describe("NBTTag") {
    it("toString from NBTTagCompound") {
      val tag = NBTTagCompound(
        Map(
          "a" -> NBTTagCompound(
            Map(
              "x" -> NBTTagByte(1),
              "y" -> NBTTagFloat(2)
            )),
          "b" -> NBTTagCompoundList(
            List(
              NBTTagCompound(Map("x" -> NBTTagInt(1))),
              NBTTagCompound(Map("y" -> NBTTagInt(2), "z" -> NBTTagInt(3)))
            ))
        ))

      val result = tag.toString
      val expects = SNBTStructure
        .VCompound(
          Map(
            "a" -> SNBTStructure.VCompound(
              Map(
                "x" -> SNBTStructure.VLiteral("1b"),
                "y" -> SNBTStructure.VLiteral("2f")
              )),
            "b" -> SNBTStructure.VList(List(
              SNBTStructure.VCompound(Map(
                "x" -> SNBTStructure.VLiteral("1")
              )),
              SNBTStructure.VCompound(Map(
                "y" -> SNBTStructure.VLiteral("2"),
                "z" -> SNBTStructure.VLiteral("3")
              ))
            ))
          ))
        .allPossibleRepresentations

      assert(expects.contains(result))
    }

    it("toString from NBTTagString") {
      val tag = NBTTagString("test")
      val result = tag.toString
      val expect = """"test""""

      assert(result == expect)
    }

    it("toString from NBTTagString with contains quotation") {
      val tag = NBTTagString(
        """{'text':'{\'text\':\'test\'}','interpret':true},{"text":"{\"text\":\"test\"}","interpret":true}""")
      val result = tag.toString
      val expect =
        """"{'text':'{\\'text\\':\\'test\\'}','interpret':true},{\"text\":\"{\\\"text\\\":\\\"test\\\"}\",\"interpret\":true}""""

      assert(result == expect)
    }

    it("toString from NBTTagByte") {
      val tag = NBTTagByte(1)
      val result = tag.toString
      val expect = "1b"

      assert(result == expect)
    }

    it("toString from NBTTagShort") {
      val tag = NBTTagShort(1)
      val result = tag.toString
      val expect = "1s"

      assert(result == expect)
    }

    it("toString from NBTTagInt") {
      val tag = NBTTagInt(1)
      val result = tag.toString
      val expect = "1"

      assert(result == expect)
    }

    it("toString from NBTTagLong") {
      val tag = NBTTagLong(1)
      val result = tag.toString
      val expect = "1L"

      assert(result == expect)
    }

    it("toString from NBTTagFloat") {
      val tag = NBTTagFloat(1.125)
      val result = tag.toString
      val expect = "1.125f"

      assert(result == expect)
    }

    it("toString from NBTTagDouble") {
      val tag = NBTTagDouble(1.125)
      val result = tag.toString
      val expect = "1.125d"

      assert(result == expect)
    }

    it("toString from NBTTagCompoundList") {
      val tag = NBTTagCompoundList(
        List(
          NBTTagCompound(
            Map(
              "y" -> NBTTagInt(2),
              "x" -> NBTTagInt(1)
            )),
          NBTTagCompound(
            Map(
              "a" -> NBTTagString("b")
            ))
        ))
      val result = tag.toString
      val expects = SNBTStructure
        .VList(
          List(
            SNBTStructure.VCompound(
              Map(
                "x" -> SNBTStructure.VLiteral("1"),
                "y" -> SNBTStructure.VLiteral("2")
              )),
            SNBTStructure.VCompound(
              Map(
                "a" -> SNBTStructure.VLiteral(""""b"""")
              ))
          ))
        .allPossibleRepresentations

      assert(expects.contains(result))
    }

    it("toString from NBTTagStringList") {
      val tag = NBTTagStringList(List(
        NBTTagString("a"),
        NBTTagString("b"),
        NBTTagString("c")
      ))
      val result = tag.toString
      val expect = """["a","b","c"]"""

      assert(result == expect)
    }

    it("toString from NBTTagByteList") {
      val tag = NBTTagByteList(List(
        NBTTagByte(1),
        NBTTagByte(2),
        NBTTagByte(3)
      ))
      val result = tag.toString
      val expect = """[B;1b,2b,3b]"""

      assert(result == expect)
    }

    it("toString from NBTTagShortList") {
      val tag = NBTTagShortList(List(
        NBTTagShort(1),
        NBTTagShort(2),
        NBTTagShort(3)
      ))
      val result = tag.toString
      val expect = """[1s,2s,3s]"""

      assert(result == expect)
    }

    it("toString from NBTTagIntList") {
      val tag = NBTTagIntList(List(
        NBTTagInt(1),
        NBTTagInt(2),
        NBTTagInt(3)
      ))
      val result = tag.toString
      val expect = """[I;1,2,3]"""

      assert(result == expect)
    }

    it("toString from NBTTagLongList") {
      val tag = NBTTagLongList(List(
        NBTTagLong(1),
        NBTTagLong(2),
        NBTTagLong(3)
      ))
      val result = tag.toString
      val expect = """[L;1L,2L,3L]"""

      assert(result == expect)
    }

    it("toString from NBTTagFloatList") {
      val tag = NBTTagFloatList(List(
        NBTTagFloat(1),
        NBTTagFloat(2),
        NBTTagFloat(3)
      ))
      val result = tag.toString
      val expect = """[1f,2f,3f]"""

      assert(result == expect)
    }

    it("toString from NBTTagDoubleList") {
      val tag = NBTTagDoubleList(List(
        NBTTagDouble(1),
        NBTTagDouble(2),
        NBTTagDouble(3)
      ))
      val result = tag.toString
      val expect = """[1d,2d,3d]"""

      assert(result == expect)
    }

    it("toString from NBTTagNestedList") {
      val tag = NBTTagNestedList(List(
        NBTTagByteList(List(
          NBTTagByte(1),
          NBTTagByte(2)
        )),
        NBTTagIntList(List(
          NBTTagInt(3),
          NBTTagInt(4)
        )),
        NBTTagLongList(List(
          NBTTagLong(5),
          NBTTagLong(6)
        ))
      ))
      val result = tag.toString
      val expect = """[[B;1b,2b],[I;3,4],[L;5L,6L]]"""

      assert(result == expect)
    }
  }

  enum SNBTStructure {
    case VLiteral(repr: String)
    case VLiteralList(prefix: String, items: List[String])
    case VList(items: List[SNBTStructure])
    case VCompound(map: Map[String, SNBTStructure])

    // [{"a":1, "b": 1},{"a":1, "b": 1}]
    // a: List(b: List) => b
    // List[List[A]] => (List[A] => A) => List[A]
    def allPossibleRepresentations: List[String] = {
      this match {
        case VLiteral(repr) => List(repr)
        case VLiteralList(prefix, items) =>
          List(items.mkString(s"[$prefix", ",", "]"))
        case VList(items) =>
          items
            .traverse(_.allPossibleRepresentations)
            .map(_.mkString("[", ",", "]"))
        case VCompound(map) =>
          map
            .mapValues(_.allPossibleRepresentations)
            .toList
            .traverse { case (key, reprs) =>
              reprs.map(repr => s""""$key":$repr""")
            }
            .flatMap(_.permutations.toList)
            .map(_.mkString("{", ",", "}"))
      }
    }
  }
}
