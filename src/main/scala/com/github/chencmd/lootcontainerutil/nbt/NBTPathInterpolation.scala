package com.github.chencmd.lootcontainerutil.nbt

import definition.{NBTPath, NBTTag}

import definition.NBTTag.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

import cats.implicits.given

object NBTPathInterpolation {
  def apply(interpolationString: String): Either[String, NBTPathInterpolation] =
    Parser.parse(interpolationString)

  private object Parser extends RegexParsers {

    override def skipWhitespace: Boolean = false

    def parse(input: String): Either[String, NBTPathInterpolation] = {
      parseAll(wholeInterpolatableString, input) match {
        case Success(res, _) => Right(res)
        case Failure(mes, _) => Left(mes)
        case Error(mes, _)   => Left(mes)
      }
    }

    private def wholeInterpolatableString: Parser[NBTPathInterpolation] = {
      for {
        _ <- not("^$".r)
        firstPart <- nextTextPart
        rest <- rep {
          for {
            // nextTextPart のうち untilNextDelimiter が成功していた場合は % が一つ残っている。
            // そうでない場合はこの読み飛ばしは fail し、 rep が止まる
            _ <- "%"
            // path が fail したら rep 全体も Error にしたい
            p <- commit(path)
            // 次の delimiter の半分まで、もしくは最後まで読む
            t <- nextTextPart
          } yield (p, t)
        }
      } yield NBTPathInterpolation(firstPart, rest)
    }

    private def nextTextPart: Parser[String] =
      untilNextDelimiter | untilTheEnd

    private def untilTheEnd: Parser[String] =
      ".*$".r.map(_.replace("\\%", "%"))

    // % を一個残してパースする
    private def untilNextDelimiter: Parser[String] =
      ".*?(?<!\\\\)%(?=%)".r.map(_.dropRight(1).replace("\\%", "%"))

    // パス部分の最初の %% が読み飛ばされたところから、次の %% までを読んで %% を吹き飛ばしたものをパスと解釈する
    private def path: Parser[NBTPath] = {
      for {
        pathString <- untilNextDelimiter
        nbtPath <- NBTPathParser.parse(pathString) match {
          case Left(err)   => failure(s"Failed parsing NBTPath: $err")
          case Right(path) => success(path)
        }
        _ <- "%"
      } yield nbtPath
    }
  }
}

case class NBTPathInterpolation(
    firstPart: String,
    pairRest: List[(NBTPath, String)])
    derives CanEqual {
  def interpolate(compound: NBTTagCompound): Option[String] =
    pairRest
      .traverse { case (path: NBTPath, str) =>
        val data = path.access(compound)
        Option.when(data.nonEmpty) {
          data.map(_.toString).mkString(",") + str
        }
      }
      .map(_.foldLeft(firstPart)(_ + _))
}
