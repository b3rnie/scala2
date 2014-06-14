package bittorrent

object Bencoding{
  abstract class Entry
  case class Int(val int : scala.Int)           extends Entry
  case class List(val list : scala.List[Entry]) extends Entry
  case class Dict(val dict : Map[String,Entry]) extends Entry
  case class Bytes(val bytes : Array[Byte])     extends Entry

  case class DecodeException(s : String) extends Exception {
    override def getMessage = s
  }

  def decode(list : scala.List[Byte]) : Entry = {
    var(e, xs) = dec(list)
    if(xs.size != 0)
      throw DecodeException(xs.size + " bytes not decoded")
    e
  }

  def dec(list : scala.List[Byte]) : (Entry, scala.List[Byte]) = {
    list match {
      case 'i' :: xs => dec_int(xs,   scala.List())
      case 'l' :: xs => dec_list(xs,  scala.List())
      case 'd' :: xs => dec_dict(xs,  Map())
      case xs        => dec_bytes(xs, scala.List())
    }
  }

  def dec_int(list : scala.List[Byte], acc : scala.List[Char]) : (Int, scala.List[Byte]) = {
    list match {
      case 'e' :: xs => (Int(acc.mkString.toInt), xs)
      case n   :: xs => dec_int(xs, acc :+ n.toChar)
    }
  }

  def dec_list(list : scala.List[Byte], acc : scala.List[Entry]) : (List, scala.List[Byte]) = {
    list match {
      case 'e' :: xs => (List(acc), xs)
      case xs0       => val (e, xs) = dec(xs0)
                        dec_list(xs, acc :+ e)
    }
  }

  def dec_dict(list : scala.List[Byte], acc : Map[String, Entry]) : (Dict, scala.List[Byte]) = {
    list match {
      case 'e' :: xs => (Dict(acc), xs)
      case xs0       => val (Bytes(k), xs1) = dec_bytes(xs0, scala.List())
                        val (v, xs)   = dec(xs1)
                        dec_dict(xs, acc + (new String(k) -> v))
    }
  }

  def dec_bytes(list : scala.List[Byte], acc : scala.List[Char])
    : (Bytes, scala.List[Byte]) = {
    list match {
      case ':' :: xs0 => var n = acc.mkString.toInt
                         var (bytes, xs) = xs0.splitAt(n)
                         (Bytes(bytes.toArray), xs)
      case n :: xs    => dec_bytes(xs, acc :+ n.toChar)
    }
  }

  def encode(e : Entry) : scala.List[Byte] = {
    e match {
      case Int(i) =>
        ('i' + i.toString + 'e').getBytes.toList
      case List(l) =>
        var res = l.map(e => encode(e)).toArray.flatten
        ('l'.toByte +: res :+ 'e'.toByte).toList
      case Bytes(b) =>
        val len = b.size.toString.getBytes :+ ':'.toByte
        (len ++ b).toList
      case Dict(m) =>
        var res = m.toArray.sortWith((a, b) => a._1 < b._1).map(
          e =>
            (e._1.length.toString.getBytes :+ ':'.toByte) ++
            e._1.getBytes ++
            encode(e._2)).flatten
        ('d'.toByte +: res :+ 'e'.toByte).toList
    }
  }
}
