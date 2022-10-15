package nbtpath

import com.github.chencmd.lootcontainerutil.nbtpath.NBTPathParser
import com.github.chencmd.lootcontainerutil.nbtpath.definition.{
  NBTPath,
  NBTPathRootNode,
  NBTTag,
  NBTPathNode
}
import org.scalatest.funspec.AnyFunSpec

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

      it("hoge") {
        val result = NBTPathParser.parse("a{list:[[0,1,2,3],[a,b,c]]}")
        val expect = NBTPath(
          NBTPathRootNode.MatchObject(
            "a",
            NBTTagCompound(
              Map(
                (
                  "list",
                  NBTTag.NBTTagNestedList(
                    List(
                      NBTTagIntList(List(0, 1, 2, 3)),
                      NBTTagStringList(List("a", "b", "c"))
                    )))
              ))
          ),
          List.empty
        )

        assert(result.exists(_ == expect))
      }

      it("Should parse success") {
        val result = NBTPathParser.parse(
          "root{a:1,b:true}.hoge[2].fuga[].piyo{test:[I;0,1,2,3]}[{x:{},y:{z:[B;1b,2B,3b]}}]")
        val expect = NBTPath(
          NBTPathRootNode.MatchObject(
            "root",
            NBTTagCompound(
              Map(
                ("a", NBTTag.NBTTagInt(1)),
                ("b", NBTTag.NBTTagByte(1))
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
              NBTTagCompound(
                Map(
                  ("test", NBTTagIntList(List(0, 1, 2, 3)))
                )
              )
            ),
            NBTPathNode.MatchElement(
              NBTTagCompound(
                Map(
                  ("x", NBTTagCompound(Map.empty)),
                  (
                    "y",
                    NBTTagCompound(
                      Map(
                        ("z", NBTTag.NBTTagByteList(List(1, 2, 3)))
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
