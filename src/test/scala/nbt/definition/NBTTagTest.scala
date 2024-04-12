package nbt.definition

import org.scalatest.funspec.AnyFunSpec

import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag

import cats.implicits.toTraverseOps

import scala.language.adhocExtensions
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTNel
import com.github.chencmd.lootcontainerutil.generic.extensions.MapExt.*
import cats.data.NonEmptyList

class NBTTagTest extends AnyFunSpec {
  import NBTTag.*
  describe("NBTTag") {
    it("toSNBT from NBTTagCompound") {
      val tag = NBTTagCompound(
        Map(
          "a" -> NBTTagCompound(
            Map(
              "x" -> NBTTagByte(1),
              "y" -> NBTTagFloat(2)
            )
          ),
          "b" -> NBTTag.listFrom(
            NBTNel.Compound(
              NonEmptyList.of(
                NBTTagCompound(Map("x" -> NBTTagInt(1))),
                NBTTagCompound(Map("y" -> NBTTagInt(2), "z" -> NBTTagInt(3)))
              )
            )
          )
        )
      )

      val result  = tag.toSNBT
      val expects = SNBTStructure
        .VCompound(
          Map(
            "a" -> SNBTStructure.VCompound(
              Map(
                "x" -> SNBTStructure.VLiteral("1b"),
                "y" -> SNBTStructure.VLiteral("2f")
              )
            ),
            "b" -> SNBTStructure.VList(
              List(
                SNBTStructure.VCompound(
                  Map(
                    "x" -> SNBTStructure.VLiteral("1")
                  )
                ),
                SNBTStructure.VCompound(
                  Map(
                    "y" -> SNBTStructure.VLiteral("2"),
                    "z" -> SNBTStructure.VLiteral("3")
                  )
                )
              )
            )
          )
        )
        .allPossibleRepresentations

      assert(expects.contains(result))
    }

    it("toSNBT from NBTTagString") {
      val tag    = NBTTagString("test")
      val result = tag.toSNBT
      val expect = """"test""""

      assert(result == expect)
    }

    it("toSNBT from NBTTagString with contains quotation") {
      val tag    = NBTTagString(
        """{'text':'{\'text\':\'test\'}','interpret':true},{"text":"{\"text\":\"test\"}","interpret":true}"""
      )
      val result = tag.toSNBT
      val expect =
        """"{'text':'{\\'text\\':\\'test\\'}','interpret':true},{\"text\":\"{\\\"text\\\":\\\"test\\\"}\",\"interpret\":true}""""

      assert(result == expect)
    }

    it("toSNBT from NBTTagByte") {
      val tag    = NBTTagByte(1)
      val result = tag.toSNBT
      val expect = "1b"

      assert(result == expect)
    }

    it("toSNBT from NBTTagShort") {
      val tag    = NBTTagShort(1)
      val result = tag.toSNBT
      val expect = "1s"

      assert(result == expect)
    }

    it("toSNBT from NBTTagInt") {
      val tag    = NBTTagInt(1)
      val result = tag.toSNBT
      val expect = "1"

      assert(result == expect)
    }

    it("toSNBT from NBTTagLong") {
      val tag    = NBTTagLong(1)
      val result = tag.toSNBT
      val expect = "1L"

      assert(result == expect)
    }

    it("toSNBT from NBTTagFloat") {
      val tag    = NBTTagFloat(1.125)
      val result = tag.toSNBT
      val expect = "1.125f"

      assert(result == expect)
    }

    it("toSNBT from NBTTagDouble") {
      val tag    = NBTTagDouble(1.125)
      val result = tag.toSNBT
      val expect = "1.125d"

      assert(result == expect)
    }

    it("toSNBT from NBTTagCompoundList") {
      val tag     = NBTTag.listFrom(
        NBTNel.Compound(
          NonEmptyList.of(
            NBTTagCompound(
              Map(
                "y" -> NBTTagInt(2),
                "x" -> NBTTagInt(1)
              )
            ),
            NBTTagCompound(
              Map(
                "a" -> NBTTagString("b")
              )
            )
          )
        )
      )
      val result  = tag.toSNBT
      val expects = SNBTStructure
        .VList(
          List(
            SNBTStructure.VCompound(
              Map(
                "x" -> SNBTStructure.VLiteral("1"),
                "y" -> SNBTStructure.VLiteral("2")
              )
            ),
            SNBTStructure.VCompound(
              Map(
                "a" -> SNBTStructure.VLiteral(""""b"""")
              )
            )
          )
        )
        .allPossibleRepresentations

      assert(expects.contains(result))
    }

    it("toSNBT from NBTTagStringList") {
      val tag    = NBTTag.listFrom(
        NBTNel.String(NonEmptyList.of("a", "b", "c").map(NBTTagString.apply))
      )
      val result = tag.toSNBT
      val expect = """["a","b","c"]"""

      assert(result == expect)
    }

    it("toSNBT from NBTTagByteArray") {
      val tag    = NBTTag.NBTTagByteArray(Vector(1, 2, 3).map(i => NBTTagByte(i.toByte)))
      val result = tag.toSNBT
      val expect = """[B;1b,2b,3b]"""

      assert(result == expect)
    }

    it("toSNBT from NBTTagIntArray") {
      val tag    = NBTTag.NBTTagIntArray(Vector(1, 2, 3).map(NBTTagInt.apply))
      val result = tag.toSNBT
      val expect = """[I;1,2,3]"""

      assert(result == expect)
    }

    it("toSNBT from NBTTagLongArray") {
      val tag    = NBTTag.NBTTagLongArray(Vector(1, 2, 3).map(i => NBTTagLong(i.toLong)))
      val result = tag.toSNBT
      val expect = """[L;1L,2L,3L]"""

      assert(result == expect)
    }

    it("toSNBT from NBTTagByteList") {
      val tag    = NBTTag.listFrom(NBTNel.Byte(NonEmptyList.of(1, 2, 3).map(i => NBTTagByte(i.toByte))))
      val result = tag.toSNBT
      val expect = """[1b,2b,3b]"""

      assert(result == expect)
    }

    it("toSNBT from NBTTagShortList") {
      val tag    = NBTTag.listFrom(
        NBTNel.Short(NonEmptyList.of(1, 2, 3).map(i => NBTTagShort(i.toShort)))
      )
      val result = tag.toSNBT
      val expect = """[1s,2s,3s]"""

      assert(result == expect)
    }

    it("toSNBT from NBTTagIntList") {
      val tag    = NBTTag.listFrom(
        NBTNel.Int(NonEmptyList.of(1, 2, 3).map(NBTTagInt.apply))
      )
      val result = tag.toSNBT
      val expect = """[1,2,3]"""

      assert(result == expect)
    }

    it("toSNBT from NBTTagLongList") {
      val tag    = NBTTag.listFrom(NBTNel.Long(NonEmptyList.of(1, 2, 3).map(i => NBTTagLong(i.toLong))))
      val result = tag.toSNBT
      val expect = """[1L,2L,3L]"""

      assert(result == expect)
    }

    it("toSNBT from NBTTagFloatList") {
      val tag    = NBTTag.listFrom(NBTNel.Float(NonEmptyList.of(1, 2, 3).map(i => NBTTagFloat(i.toFloat))))
      val result = tag.toSNBT
      val expect = """[1f,2f,3f]"""

      assert(result == expect)
    }

    it("toSNBT from NBTTagDoubleList") {
      val tag    = NBTTag.listFrom(NBTNel.Double(NonEmptyList.of(1, 2, 3).map(i => NBTTagDouble(i.toDouble))))
      val result = tag.toSNBT
      val expect = """[1d,2d,3d]"""

      assert(result == expect)
    }

    it("toSNBT from NBTTagNestedList") {
      val tag    = NBTTag.listFrom(
        NBTNel.List(
          NonEmptyList.of(
            NBTTag.listFrom(),
            NBTTag.listFrom(NBTNel.Byte(NonEmptyList.of(1, 2).map(i => NBTTagByte(i.toByte)))),
            NBTTag.listFrom(NBTNel.Short(NonEmptyList.of(3, 4).map(i => NBTTagShort(i.toShort)))),
            NBTTag.listFrom(NBTNel.Int(NonEmptyList.of(5, 6).map(NBTTagInt.apply))),
            NBTTag.listFrom(NBTNel.Long(NonEmptyList.of(7, 8).map(i => NBTTagLong(i.toLong)))),
            NBTTag.listFrom(NBTNel.String(NonEmptyList.of("9", "10").map(i => NBTTagString(i)))),
            NBTTag.listFrom(NBTNel.Compound(NonEmptyList.of(NBTTagCompound(Map("x" -> NBTTagInt(11))))))
          )
        )
      )
      val result = tag.toSNBT
      val expect = """[[],[1b,2b],[3s,4s],[5,6],[7L,8L],["9","10"],[{"x":11}]]"""

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
        case VLiteral(repr)              => List(repr)
        case VLiteralList(prefix, items) => List(items.mkString(s"[$prefix", ",", "]"))
        case VList(items)                => items
            .traverse(_.allPossibleRepresentations)
            .map(_.mkString("[", ",", "]"))
        case VCompound(map)              => map
            .mapV(_.allPossibleRepresentations)
            .toList
            .traverse { case (key, reprs) => reprs.map(repr => s""""$key":$repr""") }
            .flatMap(_.permutations.toList)
            .map(_.mkString("{", ",", "}"))
      }
    }
  }
}
