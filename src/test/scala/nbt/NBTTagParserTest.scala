package nbt

import com.github.chencmd.lootcontainermanager.nbt.NBTTagParser
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTTag
import org.scalatest.funspec.AnyFunSpec

import scala.language.adhocExtensions
import com.github.chencmd.lootcontainermanager.nbt.definition.NBTNel
import cats.data.NonEmptyList
import com.github.chencmd.lootcontainermanager.nbt.definition.{NBTNel, NBTTag}
import com.github.chencmd.lootcontainermanager.nbt.NBTTagParser

class NBTTagParserTest extends AnyFunSpec {
  import NBTTag.*
  describe("NBTTagParser") {
    it("should parse NBTTagByte") {
      val input    = "{key:1b}"
      val expected = NBTTagCompound(Map("key" -> NBTTagByte(1)))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagShort") {
      val input    = "{key:1s}"
      val expected = NBTTagCompound(Map("key" -> NBTTagShort(1)))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagInt") {
      val input    = "{key:1}"
      val expected = NBTTagCompound(Map("key" -> NBTTagInt(1)))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagLong") {
      val input    = "{key:1L}"
      val expected = NBTTagCompound(Map("key" -> NBTTagLong(1)))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagFloat") {
      val input    = "{key:1.0f}"
      val expected = NBTTagCompound(Map("key" -> NBTTagFloat(1.0f)))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagFloat without integer part") {
      val input    = "{key:.25f}"
      val expected = NBTTagCompound(Map("key" -> NBTTagFloat(0.25f)))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagDouble") {
      val input    = "{key:1.0d}"
      val expected = NBTTagCompound(Map("key" -> NBTTagDouble(1.0)))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagDouble with non suffix") {
      val input    = "{key:-1.0}"
      val expected = NBTTagCompound(Map("key" -> NBTTagDouble(-1.0)))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagDouble with non suffix / without integer part") {
      val input    = "{key:.25}"
      val expected = NBTTagCompound(Map("key" -> NBTTagDouble(0.25)))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagString with single quote") {
      val input    = """{key:' " \' " \\\' 1 \\\' " \' " '}"""
      val expected = NBTTagCompound(Map("key" -> NBTTagString(""" " ' " \' 1 \' " ' " """)))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagString with double quote") {
      val input    = """{key:" ' \" ' \\\" 1 \\\" ' \" ' "}"""
      val expected = NBTTagCompound(Map("key" -> NBTTagString(""" ' " ' \" 1 \" ' " ' """)))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagByteArray") {
      val input    = "{key:[B; 1b,2b]}"
      val expected = NBTTagCompound(Map("key" -> NBTTagByteArray(Vector(NBTTagByte(1), NBTTagByte(2)))))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagIntArray") {
      val input    = "{key:[I; 1,2]}"
      val expected = NBTTagCompound(Map("key" -> NBTTagIntArray(Vector(NBTTagInt(1), NBTTagInt(2)))))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagLongArray") {
      val input    = "{key:[L;1L,2L]}"
      val expected = NBTTagCompound(Map("key" -> NBTTagLongArray(Vector(NBTTagLong(1), NBTTagLong(2)))))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagList with empty") {
      val input    = "{key:[]}"
      val expected = NBTTagCompound(Map("key" -> NBTTagList(None)))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagList with NBTTagByte") {
      val input    = "{key:[1b]}"
      val expected = NBTTagCompound(Map("key" -> listFrom(NBTNel.Byte(NonEmptyList.of(NBTTagByte(1))))))
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagList with nested NBTTagList") {
      val input    = """{key:[[1b,2b],["3","4"]]}"""
      val expected = NBTTagCompound(
        Map(
          "key" -> listFrom(
            NBTNel.List(
              NonEmptyList.of(
                listFrom(NBTNel.Byte(NonEmptyList.of(NBTTagByte(1), NBTTagByte(2)))),
                listFrom(NBTNel.String(NonEmptyList.of(NBTTagString("3"), NBTTagString("4"))))
              )
            )
          )
        )
      )
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
    it("should parse NBTTagCompound") {
      val input    = "{}"
      val expected = NBTTagCompound(Map.empty)
      val actual   = NBTTagParser.parse(input)
      assert(actual == Right(expected))
    }
  }
}
