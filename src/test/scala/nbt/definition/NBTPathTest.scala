package nbt.definition

import org.scalatest.funspec.AnyFunSpec

import scala.language.adhocExtensions
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTTag
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTPath
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTPathRootNode as RootNode
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTPathNode as Node
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTNel
import cats.data.NonEmptyList

class NBTPathTest extends AnyFunSpec {
  import NBTTag.*
  describe("NBTPath") {
    it("should be match root") {
      val tag: NBTTagCompound = NBTTagCompound(Map("foo" -> NBTTagInt(1), "bar" -> NBTTagInt(2)))
      val path                = NBTPath(RootNode.MatchRootObject(NBTTagCompound(Map("foo" -> NBTTagInt(1)))), List())
      val expected            = List(NBTTagCompound(Map("foo" -> NBTTagInt(1), "bar" -> NBTTagInt(2))))
      assert(path.access(tag) == expected)
    }
    it("should be not match root") {
      val tag: NBTTagCompound = NBTTagCompound(Map("foo" -> NBTTagInt(1), "bar" -> NBTTagInt(2)))
      val path                = NBTPath(RootNode.MatchRootObject(NBTTagCompound(Map("foo" -> NBTTagInt(2)))), List())
      val expected            = List()
      assert(path.access(tag) == expected)
    }
    it("should be get child") {
      val tag: NBTTagCompound = NBTTagCompound(Map("foo" -> NBTTagInt(1), "bar" -> NBTTagInt(2)))
      val path                = NBTPath(RootNode.CompoundChild("foo"), List())
      val expected            = List(NBTTagInt(1))
      assert(path.access(tag) == expected)
    }
    it("should be match object") {
      val tag: NBTTagCompound = NBTTagCompound(
        Map("foo" -> NBTTagCompound(Map("bar" -> NBTTagInt(1), "baz" -> NBTTagInt(2))))
      )
      val path                = NBTPath(RootNode.MatchObject("foo", NBTTagCompound(Map("bar" -> NBTTagInt(1)))), List())
      val expected            = List(NBTTagCompound(Map("bar" -> NBTTagInt(1), "baz" -> NBTTagInt(2))))
      assert(path.access(tag) == expected)
    }
    it("should be not match object") {
      val tag: NBTTagCompound = NBTTagCompound(
        Map("foo" -> NBTTagCompound(Map("bar" -> NBTTagInt(1), "baz" -> NBTTagInt(2))))
      )
      val path                = NBTPath(RootNode.MatchObject("foo", NBTTagCompound(Map("bar" -> NBTTagInt(2)))), List())
      val expected            = List()
      assert(path.access(tag) == expected)
    }
    it("should be match object and get child") {
      val tag: NBTTagCompound = NBTTagCompound(
        Map("foo" -> NBTTagCompound(Map("bar" -> NBTTagInt(1), "baz" -> NBTTagInt(2))))
      )
      val path                = NBTPath(
        RootNode.MatchObject("foo", NBTTagCompound(Map("bar" -> NBTTagInt(1)))),
        List(Node.CompoundChild("bar"))
      )
      val expected            = List(NBTTagInt(1))
      assert(path.access(tag) == expected)
    }
    it("should be get child with all elements") {
      val tag: NBTTagCompound = NBTTagCompound(
        Map("foo" -> NBTTag.listFrom(NBTNel.Int(NonEmptyList.of(NBTTagInt(1), NBTTagInt(2)))))
      )
      val path                = NBTPath(RootNode.CompoundChild("foo"), List(Node.AllElements()))
      val expected            = List(NBTTagInt(1), NBTTagInt(2))
      assert(path.access(tag) == expected)
    }
    it("should be get child with all elements child") {
      val tag: NBTTagCompound = NBTTagCompound(
        Map(
          "foo" -> NBTTag.listFrom(
            NBTNel.Compound(
              NonEmptyList.of(
                NBTTagCompound(Map("bar" -> NBTTagInt(1), "baz" -> NBTTagInt(1))),
                NBTTagCompound(Map("bar" -> NBTTagInt(2), "baz" -> NBTTagInt(1)))
              )
            )
          )
        )
      )
      val path                = NBTPath(
        RootNode.CompoundChild("foo"),
        List(Node.AllElements(), Node.CompoundChild("bar"))
      )
      val expected            = List(NBTTagInt(1), NBTTagInt(2))
      assert(path.access(tag) == expected)
    }
    it("should be get child with match element") {
      val tag: NBTTagCompound = NBTTagCompound(
        Map(
          "foo" -> NBTTag.listFrom(
            NBTNel.Compound(
              NonEmptyList.of(
                NBTTagCompound(Map("bar" -> NBTTagInt(1), "baz" -> NBTTagInt(1))),
                NBTTagCompound(Map("bar" -> NBTTagInt(2), "baz" -> NBTTagInt(1)))
              )
            )
          )
        )
      )
      val path                = NBTPath(
        RootNode.CompoundChild("foo"),
        List(Node.MatchElement(NBTTagCompound(Map("bar" -> NBTTagInt(2)))))
      )
      val expected            = List(NBTTagCompound(Map("bar" -> NBTTagInt(2), "baz" -> NBTTagInt(1))))
      assert(path.access(tag) == expected)
    }
    it("should be get child with match elements") {
      val tag: NBTTagCompound = NBTTagCompound(
        Map(
          "foo" -> NBTTag.listFrom(
            NBTNel.Compound(
              NonEmptyList.of(
                NBTTagCompound(Map("bar" -> NBTTagInt(1), "baz" -> NBTTagInt(2))),
                NBTTagCompound(Map("bar" -> NBTTagInt(2), "baz" -> NBTTagInt(1))),
                NBTTagCompound(Map("bar" -> NBTTagInt(3), "baz" -> NBTTagInt(2)))
              )
            )
          )
        )
      )
      val path                = NBTPath(
        RootNode.CompoundChild("foo"),
        List(Node.MatchElement(NBTTagCompound(Map("baz" -> NBTTagInt(2)))))
      )
      val expected            = List(
        NBTTagCompound(Map("bar" -> NBTTagInt(1), "baz" -> NBTTagInt(2))),
        NBTTagCompound(Map("bar" -> NBTTagInt(3), "baz" -> NBTTagInt(2)))
      )
      assert(path.access(tag) == expected)
    }
    it("should be get child with indexed element") {
      val tag: NBTTagCompound = NBTTagCompound(
        Map(
          "foo" -> NBTTag.listFrom(
            NBTNel.Compound(
              NonEmptyList.of(
                NBTTagCompound(Map("bar" -> NBTTagInt(1), "baz" -> NBTTagInt(2))),
                NBTTagCompound(Map("bar" -> NBTTagInt(2), "baz" -> NBTTagInt(1))),
                NBTTagCompound(Map("bar" -> NBTTagInt(3), "baz" -> NBTTagInt(2)))
              )
            )
          )
        )
      )
      val path                = NBTPath(
        RootNode.CompoundChild("foo"),
        List(Node.IndexedElement(1))
      )
      val expected            = List(NBTTagCompound(Map("bar" -> NBTTagInt(2), "baz" -> NBTTagInt(1))))
      assert(path.access(tag) == expected)
    }
    it("should be get child with compound child") {
      val tag: NBTTagCompound = NBTTagCompound(
        Map(
          "foo" -> NBTTagCompound(
            Map(
              "bar" -> NBTTagInt(1),
              "baz" -> NBTTagInt(2),
              "qux" -> NBTTagCompound(Map("quux" -> NBTTagInt(3)))
            )
          )
        )
      )
      val path                = NBTPath(
        RootNode.CompoundChild("foo"),
        List(Node.CompoundChild("qux"))
      )
      val expected            = List(NBTTagCompound(Map("quux" -> NBTTagInt(3))))
      assert(path.access(tag) == expected)
    }
  }
}
