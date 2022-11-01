package nbt

import com.github.chencmd.lootcontainerutil.nbt.NBTPathInterpolationParser
import com.github.chencmd.lootcontainerutil.nbt.definition.{NBTPath, NBTPathInterpolation, NBTPathRootNode}
import org.scalatest.funspec.AnyFunSpec

class NBTPathInterpolationParserTest extends AnyFunSpec {
  import NBTPathRootNode.*
  describe("NBTPathInterpolationParser") {
    it("parse no contain interpolation") {
      val result = NBTPathInterpolationParser.parse("text")
      val expect = Right(NBTPathInterpolation("text", List.empty))

      assert(result == expect)
    }

    it("parse one contain interpolation") {
      val result = NBTPathInterpolationParser.parse("text%%path%%text2")
      val expect = Right(NBTPathInterpolation(
        "text",
        List(NBTPath(CompoundChild("path"), List.empty) -> "text2")
      ))

      assert(result == expect)
    }

    it("parse two contain interpolation") {
      val result = NBTPathInterpolationParser.parse("text%%path%%text2%%path2%%text3")
      val expect = Right(NBTPathInterpolation(
        "text",
        List(
          NBTPath(CompoundChild("path"), List.empty) -> "text2",
          NBTPath(CompoundChild("path2"), List.empty) -> "text3"
        )
      ))

      assert(result == expect)
    }

    it("parse contain single percent char") {
      val result = NBTPathInterpolationParser.parse("%")
      val expect = Right(NBTPathInterpolation(
        "%",
        List.empty
      ))

      assert(result == expect)
    }

    it("parse contain escape percent char") {
      val result = NBTPathInterpolationParser.parse("""\%%""")
      val expect = Right(NBTPathInterpolation(
        "%%",
        List.empty
      ))

      assert(result == expect)
    }

    it("parse escape percent char") {
      val result = NBTPathInterpolationParser.parse("""\%\%%%path%%\%\%""")
      val expect = Right(NBTPathInterpolation(
        "%%",
        List(NBTPath(CompoundChild("path"), List.empty) -> "%%")
      ))

      assert(result == expect)
    }

    it("parse only interpolation") {
      val result = NBTPathInterpolationParser.parse("%%path%%")
      val expect = Right(NBTPathInterpolation(
        "",
        List(NBTPath(CompoundChild("path"), List.empty) -> "")
      ))

      assert(result == expect)
    }

    it("failed empty string") {
      val result = NBTPathInterpolationParser.parse("")

      assert(result.isLeft)
    }

    it("failed no ended delimiter") {
      val result = NBTPathInterpolationParser.parse("text%%")

      assert(result.isLeft)
    }

    it("failed failed parse NBTPath") {
      val result = NBTPathInterpolationParser.parse("%%{%%")

      assert(result.isLeft)
    }
  }
}
