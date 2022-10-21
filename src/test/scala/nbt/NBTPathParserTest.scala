package nbt

import com.github.chencmd.lootcontainerutil.nbt.NBTPathParser
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTTag.NBTTagIntList
import com.github.chencmd.lootcontainerutil.nbt.definition.{
  NBTPath,
  NBTPathNode,
  NBTPathRootNode,
  NBTTag
}
import org.scalatest.funspec.AnyFunSpec
import scala.util.chaining.*

class NBTPathParserTest extends AnyFunSpec {
  describe("NBTPathParser") {
    import NBTTag.*
    describe("RootNode") {
      it("Should parse succeed CompoundChild") {
        val result = NBTPathParser.parse("root")
        val expect = NBTPath(NBTPathRootNode.CompoundChild("root"), List.empty)

        assert(result.exists(_ == expect))
      }

      it("Should parse succeed CompoundChild with single quoted") {
        val result = NBTPathParser.parse("'root'")
        val expect = NBTPath(NBTPathRootNode.CompoundChild("root"), List.empty)

        assert(result.exists(_ == expect))
      }

      it("Should parse succeed CompoundChild with double quoted") {
        val result = NBTPathParser.parse("\"root\"")
        val expect = NBTPath(NBTPathRootNode.CompoundChild("root"), List.empty)

        assert(result.exists(_ == expect))
      }

      it("Strings contains whitespace should successful to parse if quoted") {
        val result = NBTPathParser.parse("'contains space'")
        val expect =
          NBTPath(NBTPathRootNode.CompoundChild("contains space"), List.empty)

        assert(result.exists(_ == expect))
      }

      it("Strings contains whitespace should fail to parse if not quoted") {
        val result = NBTPathParser.parse("contains space")

        assert(result.isLeft)
      }

      it("Should parse succeed empty MatchRootObject") {
        val result = NBTPathParser.parse("{}")
        val expect = NBTPath(
          NBTPathRootNode.MatchRootObject(NBTTagCompound(Map.empty)),
          List.empty
        )

        assert(result.exists(_ == expect))
      }

      it("Should parse succeed empty MatchChildObject") {
        val result = NBTPathParser.parse("root{}")
        val expect = NBTPath(
          NBTPathRootNode.MatchObject("root", NBTTagCompound(Map.empty)),
          List.empty)

        assert(result.exists(_ == expect))
      }

      it("Should parse failed indexed access") {
        val result = NBTPathParser.parse("[0]")

        assert(result.isLeft)
      }

      it("Should parse failed foreach access") {
        val result = NBTPathParser.parse("[]")

        assert(result.isLeft)
      }

      it("Should parse") {
        val result = NBTPathParser.parse("a{list:[0,1,2]}")
        val expect = NBTPath(
          NBTPathRootNode.MatchObject(
            "a",
            NBTTagCompound(
              Map(
                "list" -> NBTTagIntList(
                  List(
                    NBTTagInt(0),
                    NBTTagInt(1),
                    NBTTagInt(2)
                  )
                )
              )
            )
          ),
          List.empty
        )

        assert(result.exists(_ == expect))
      }

      it("Should parse NestedList") {
        val result = NBTPathParser.parse("a{list:[[0,1,2]]}")
        val expect = NBTPath(
          NBTPathRootNode.MatchObject(
            "a",
            NBTTagCompound(
              Map(
                "list" -> NBTTagNestedList(
                  List(
                    NBTTagIntList(
                      List(
                        NBTTagInt(0),
                        NBTTagInt(1),
                        NBTTagInt(2)
                      )
                    )
                  )
                )
              )
            )
          ),
          List.empty
        )

        assert(result.exists(_ == expect))
      }

      it("Should parse success") {
        val result = NBTPathParser.parse("root{b:[B;1b,2B,3b]}")
        val expect = NBTPath(
          NBTPathRootNode.MatchObject(
            "root",
            NBTTagCompound(
              Map(
                "b" -> NBTTagByteList(
                  List(NBTTagByte(1), NBTTagByte(2), NBTTagByte(3))
                )
              )
            )
          ),
          List.empty
        )

        assert(result.exists(_ == expect))
      }
    }
  }
}
