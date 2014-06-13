package bittorrent

object Bencoding{
  abstract class Bentry
  case class Bint(val int : Int) extends Bentry
  case class Blist(val list : List[Bentry]) extends Bentry
  case class Bdict(val dict : Map[String, Bentry]) extends Bentry
  case class Bbytes(val bytes : Array[Byte]) extends Bentry

  case class Bexception(s : String) extends Exception {
    override def getMessage = s
  }

  def decode(list : List[Byte]) : Bentry = {
    var(e, xs) = dec(list)
    if(xs.size != 0)
      throw Bexception(xs.size + " bytes not decoded")
    e
  }

  def dec(list : List[Byte]) : (Bentry, List[Byte]) = {
    list match {
      case 'i' :: xs => dec_int(xs,   List())
      case 'l' :: xs => dec_list(xs,  List())
      case 'd' :: xs => dec_dict(xs,  Map())
      case xs        => dec_bytes(xs, List())
    }
  }

  def dec_int(list : List[Byte], acc : List[Char]) : (Bint, List[Byte]) = {
    list match {
      case 'e' :: xs => (Bint(acc.mkString.toInt), xs)
      case n   :: xs => dec_int(xs, acc :+ n.toChar)
    }
  }

  def dec_list(list : List[Byte], acc : List[Bentry]) : (Blist, List[Byte]) = {
    list match {
      case 'e' :: xs => (Blist(acc), xs)
      case xs0       => val (e, xs) = dec(xs0)
                        dec_list(xs, acc :+ e)
    }
  }

  def dec_dict(list : List[Byte], acc : Map[String, Bentry]) : (Bdict, List[Byte]) = {
    list match {
      case 'e' :: xs => (Bdict(acc), xs)
      case xs0       => val (Bbytes(k), xs1) = dec_bytes(xs0, List())
                        val (v, xs)   = dec(xs1)
                        dec_dict(xs, acc + (new String(k) -> v))
    }
  }

  def dec_bytes(list : List[Byte], acc : List[Char]) : (Bbytes, List[Byte]) = {
    list match {
      case ':' :: xs0 => var n = acc.mkString.toInt
                         var (bytes, xs) = xs0.splitAt(n)
                         (Bbytes(bytes.toArray), xs)
      case n :: xs    => dec_bytes(xs, acc :+ n.toChar)
    }
  }

  def encode(e : Bentry) : List[Byte] = {
    e match {
      case Bint(i) =>
        ('i' + i.toString + 'e').getBytes.toList
      case Blist(l) =>
        var res = l.map(e => encode(e)).toArray.flatten
        ('l'.toByte +: res :+ 'e'.toByte).toList
      case Bbytes(b) =>
        val len = b.size.toString.getBytes :+ ':'.toByte
        (len ++ b).toList
      case Bdict(m) =>
        var res = m.toArray.sortWith((a, b) => a._1 < b._1).map(
          e =>
            (e._1.length.toString.getBytes :+ ':'.toByte) ++
            e._1.getBytes ++
            encode(e._2)).flatten
        ('d'.toByte +: res :+ 'e'.toByte).toList
    }
  }
}
