package nbt.definition

import org.scalatest.funspec.AnyFunSpec
import com.github.chencmd.lootcontainerutil.nbt.definition.*
import cats.data.NonEmptyList

import scala.language.adhocExtensions

open class NBTPathInterpolationTest extends AnyFunSpec {
  import NBTTag.*
  describe("An NBTPathInterpolation") {
    describe("when does not have interpolant") {
      it("should return the underlying string") {
        val interpolation = NBTPathInterpolation("text", List.empty)
        val result        = interpolation.interpolate(NBTTagCompound(Map.empty))
        val expect        = "text"

        assert(result.exists(_ == expect))
      }
    }

    describe("when interpolating with a deterministic path on a compound") {
      it("should substitute the value at the path to the interplolant") {
        val interpolation       = NBTPathInterpolation(
          "before",
          List(
            (NBTPath(NBTPathRootNode.CompoundChild("key"), List.empty), "after")
          )
        )
        val tag: NBTTagCompound = NBTTagCompound(
          Map(
            "key" -> NBTTagInt(1)
          )
        )
        val result              = interpolation.interpolate(tag)
        val expect              = "before1after"

        assert(result.exists(_ == expect))
      }
    }

    describe("when interpolating with a nondeterminisic path on a compound") {
      it("should substitute a concatenation of all path query results by comma to the interpolant") {
        val interpolation       = NBTPathInterpolation(
          "before",
          List(
            (
              NBTPath(
                NBTPathRootNode.CompoundChild("list"),
                List(NBTPathNode.AllElements())
              ),
              "after"
            )
          )
        )
        val tag: NBTTagCompound = NBTTagCompound(
          Map(
            "list" -> NBTTag.listFrom(NBTNel.Int(NonEmptyList.of(1, 2, 3).map(NBTTagInt.apply)))
          )
        )
        val result              = interpolation.interpolate(tag)
        val expect              = "before1,2,3after"

        assert(result.exists(_ == expect))
      }
    }

    describe("when contains an NBTPath that does not match given compound") {
      it("should fail and return a Left") {
        val interpolation       = NBTPathInterpolation(
          "before",
          List(
            (NBTPath(NBTPathRootNode.CompoundChild("key"), List.empty), "after")
          )
        )
        val tag: NBTTagCompound = NBTTagCompound(Map.empty)
        val result              = interpolation.interpolate(tag)

        assert(result.isEmpty)
      }
    }
  }
}
