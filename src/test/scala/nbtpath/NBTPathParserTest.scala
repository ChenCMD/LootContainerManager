package nbtpath

import com.github.chencmd.lootcontainerutil.nbtpath.NBTPathParser
import com.github.chencmd.lootcontainerutil.nbtpath.definition.{
  CompoundTag,
  NBTPath,
  NBTPathRootNode,
  CompoundValue,
  NBTPathNode
}
import org.scalatest.funspec.AnyFunSpec

class NBTPathParserTest extends AnyFunSpec {
  describe("NBTPathParser") {
    describe("RootNode") {
      it("Should parse succeed CompoundChild") {
        val result = NBTPathParser.parseNBTPath("root")
        val expect = NBTPath(NBTPathRootNode.CompoundChild("root"), List.empty)

        assert(result.exists(_ == expect))
      }

      it("Should parse succeed CompoundChild with single quoted") {
        val result = NBTPathParser.parseNBTPath("'root'")
        val expect = NBTPath(NBTPathRootNode.CompoundChild("root"), List.empty)

        assert(result.exists(_ == expect))
      }

      it("Should parse succeed CompoundChild with double quoted") {
        val result = NBTPathParser.parseNBTPath("\"root\"")
        val expect = NBTPath(NBTPathRootNode.CompoundChild("root"), List.empty)

        assert(result.exists(_ == expect))
      }

      it("Strings contains whitespace should successful to parse if quoted") {
        val result = NBTPathParser.parseNBTPath("'contains space'")
        val expect =
          NBTPath(NBTPathRootNode.CompoundChild("contains space"), List.empty)

        assert(result.exists(_ == expect))
      }

      it("Strings contains whitespace should fail to parse if not quoted") {
        val result = NBTPathParser.parseNBTPath("contains space")

        assert(result.isLeft)
      }

      it("Should parse succeed empty MatchRootObject") {
        val result = NBTPathParser.parseNBTPath("{}")
        val expect = NBTPath(
          NBTPathRootNode.MatchRootObject(CompoundTag(Map.empty)),
          List.empty)

        assert(result.exists(_ == expect))
      }

      it("Should parse succeed empty MatchChildObject") {
        val result = NBTPathParser.parseNBTPath("root{}")
        val expect = NBTPath(
          NBTPathRootNode.MatchObject("root", CompoundTag(Map.empty)),
          List.empty)

        assert(result.exists(_ == expect))
      }

      it("Should parse failed indexed access") {
        val result = NBTPathParser.parseNBTPath("[0]")

        assert(result.isLeft)
      }

      it("Should parse failed foreach access") {
        val result = NBTPathParser.parseNBTPath("[]")

        assert(result.isLeft)
      }

      it("Should parse success") {
        val result = NBTPathParser.parseNBTPath(
          "root{a:1,b:true}.hoge[2].fuga[].piyo{test:[I;0,1,2,3]}[{x:{},y:{z:[B;1b,2B,3b]}}]")
        val expect = NBTPath(
          NBTPathRootNode.MatchObject(
            "root",
            CompoundTag(
              Map(
                ("a", CompoundValue.VInt(1)),
                ("b", CompoundValue.VByte(1))
              )
            )
          ),
          List(
            NBTPathNode.CompoundChild("hoge"),
            NBTPathNode.IndexedElement(2),
            NBTPathNode.CompoundChild("fuga"),
            NBTPathNode.AllElements(),
            NBTPathNode.MatchObject(
              "piyo",
              CompoundTag(
                Map(
                  ("test", CompoundValue.VIntList(List(0, 1, 2, 3)))
                )
              )
            ),
            NBTPathNode.MatchElement(
              CompoundTag(
                Map(
                  ("x", CompoundValue.VCompound(CompoundTag(Map.empty))),
                  (
                    "y",
                    CompoundValue.VCompound(
                      CompoundTag(
                        Map(
                          ("z", CompoundValue.VByteList(List(1, 2, 3)))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )

        assert(result.exists(_ == expect))
      }
    }
  }
}
