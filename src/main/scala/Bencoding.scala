package bittorrent

import scala.collection.BufferedIterator

object Bencoding{
  sealed abstract class Entry
  case class Int(val int : scala.Int)           extends Entry
  case class List(val list : scala.List[Entry]) extends Entry
  case class Dict(val dict : Map[String,Entry]) extends Entry
  case class Bytes(val bytes : String)          extends Entry

  case class DecodeException(s : String) extends Exception {
    override def getMessage = s
  }

  def decode(input : scala.Array[Byte]) : Entry = {
    decode(input.iterator.buffered)
  }

  def decode(input : BufferedIterator[Byte]) : Entry = {
    try {
      input.head match {
        case c if(isIntByte(c)) =>
          decodeBytes(input)
        case _ =>
          input.next match {
            case 'i' => decodeInt(input)
            case 'l' => decodeList(input)
            case 'd' => decodeDict(input)
            case c   => throw new DecodeException("unexpected character in input")
          }
      }
    } catch {
      case e: java.util.NoSuchElementException =>
        ???
    }
  }

  def decodeInt(input : BufferedIterator[Byte],
                acc   : StringBuilder = new StringBuilder()) : Int = {
    input.next match {
      case 'e'                => Int(acc.toString.toInt)
      case c if(isIntByte(c)) => decodeInt(input, acc.append(c.toChar))
      case c                  => throw new DecodeException("unexpected character while decoding integer")
    }
  }

  def decodeList(input : BufferedIterator[Byte],
                 acc   : scala.List[Entry] = scala.List()) : List = {
    input.head match {
      case 'e' =>
        input.next
        List(acc.reverse)
      case _ =>
        val entry = decode(input)
        decodeList(input, entry :: acc)
    }
  }

  def decodeDict(input : BufferedIterator[Byte],
                 acc   : Map[String,Entry] = Map()) : Dict = {
    input.head match {
      case 'e' =>
        input.next
        Dict(acc)
      case _ =>
        val Bytes(k) = decodeBytes(input)
        val v        = decode(input)
        decodeDict(input, acc + (k -> v))
    }
  }

  def decodeBytes(input : BufferedIterator[Byte],
                  acc   : StringBuilder = new StringBuilder()) : Bytes = {
    input.next match {
      case ':' =>
        val n = acc.toString.toInt
        val b = input.take(n)
        Bytes(new String(b.toArray.map(_.toChar)))
      case c if isIntByte(c) =>
        decodeBytes(input, acc.append(c.toChar))
      case _ =>
        throw new Exception("illegal character")
    }
  }

  private def isIntByte(c : Byte) = c >= '0' && c <= '9'

  def encode(e : Entry) : scala.Array[Byte] = {
    e match {
      case Int(i) =>
        ('i' + i.toString + 'e').getBytes
      case List(l) =>
        var res = l.map(e => encode(e)).toArray.flatten
        ('l'.toByte +: res :+ 'e'.toByte)
      case Bytes(b) =>
        val len = b.length.toString + ":"
        val res = (len + b).toArray.map(_.toByte)
        res
      case Dict(m) =>
        var res = m.toArray.sortWith((a, b) => a._1 < b._1).map(e => {
            var len = (e._1.length.toString + ":" + e._1)
            len.getBytes ++ encode(e._2)
        }).flatten
        'd'.toByte +: res :+ 'e'.toByte
    }
  }
}
