package Bencode

import scala.io.{Source}
import scala.xml._

import java.io.PrintWriter
import java.io._

abstract class BencodeEntry
case class BencodeInt(val int : Int) extends BencodeEntry
case class BencodeList(val list : List[Any]) extends BencodeEntry
case class BencodeDict(val dict : Map[String,Any]) extends BencodeEntry
case class BencodeBytes(val bytes : Array[Byte]) extends BencodeEntry

object Bencode{
  def decode(list : List[Byte]) = {
    list match {
      case 'i' :: xs => decode_int(xs,   List())
      case 'l' :: xs => decode_list(xs,  List())
      case 'd' :: xs => decode_dict(xs,  Map())
      case xs        => decode_bytes(xs, List())
    }
  }

  def decode_int(list : List[Byte], acc : List[Char]) : (BencodeEntry, List[Byte]) = {
    list match {
      case 'e' :: xs => (BencodeInt(acc.mkString.toInt), xs)
      case n   :: xs => decode_int(xs, acc :+ n.toChar)
    }
  }

  def decode_list(list : List[Byte], acc : List[Any]) : (BencodeList, List[Byte]) = {
    list match {
      case 'e' :: xs => (BencodeList(acc), xs)
      case xs0       => val (e, xs) = decode(xs0)
                        println(e)
                        decode_list(xs, acc :+ e)
    }
  }

  def decode_dict(list : List[Byte], acc : Map[String, Any]) : (BencodeDict, List[Byte]) = {
    list match {
      case 'e' :: xs => (BencodeDict(acc), xs)
      case xs0       => val (BencodeBytes(k0), xs1) = decode_bytes(xs0, List())
                        val (v, xs)   = decode(xs1)
                        val k         = new String(k0)
                        decode_dict(xs, acc + (k -> v))
    }
  }

  def decode_bytes(list : List[Byte], acc : List[Char]) : (BencodeBytes, List[Byte]) = {
    list match {
      case ':' :: xs0 => var n = acc.mkString.toInt
                         var (bytes, xs) = xs0.splitAt(n)
                         (BencodeBytes(bytes.toArray), xs)
      case n :: xs    => decode_bytes(xs, acc :+ n.toChar)
    }
  }

  def encode(x : Any) : List[Byte] = {
    x match {
      case BencodeInt(i) =>
        ('i' + i.toString + 'e').getBytes.toList
      case BencodeList(l) =>
        var res = l.map(e => encode(e)).toArray.flatten
        ('l'.toByte +: res :+ 'e'.toByte).toList
      case BencodeBytes(b) =>
        val len = b.size.toString.getBytes :+ ':'.toByte
        (len ++ b).toList
      case BencodeDict(m) =>
        var res = m.toArray.map(
                    e =>
                      (e._1.length.toString.getBytes :+ ':'.toByte) ++
                      e._1.getBytes ++
                      encode(e._2)).flatten
        ('d'.toByte +: res :+ 'e'.toByte).toList
    }
  }
}

object FileUtils {
  def readFile(name : String) : List[Byte] = {
    val src = Source.fromFile(name)(scala.io.Codec.ISO8859)
    try {
      src.map(_.toByte).toList
    } finally { src.close }
  }

  def writeFile(f : String)(op : java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(new File(f), "ISO-8859-1")
    try {
      op(p)
    } finally { p.close() }
  }
}
