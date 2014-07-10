package tracker

import java.nio.ByteBuffer
import pimpme._

object DHT {
  //val md = java.security.MessageDigest.getInstance("SHA-1")
  def my_id = new Array[Byte](20)
  scala.util.Random.nextBytes(my_id)

  def distance(a : BigInt, b : BigInt) : BigInt = {
    a ^ b
  }

  def distance(a : Array[Byte], b : Array[Byte]) : BigInt = {
    require(a.size == b.size == 20)
    a.zip(b).foldLeft(BigInt(0))((acc,e) => {
      (acc << 8) + ((e._1 ^ e._2) & 0xFF)
    })
  }

  //def intToArray(int : BigInt) : Array[Byte] = {
  //  (0 to 20).map(n => (int >> n*8) & 0xFF)
  //}
}


object DHTMessage {
  /* y => q
   *   query, additional keys
   *   q - method (bytes)
   *   a - arguments (dict)
   * y => r
   *   response, additional keys
   *   r - named return values
   * y => e
   *   error, additional keys
   *   e - list (error code (int), error msg (string))
   */
  def message(e : Bencoding.Entry) : Option[Bencoding.Dict] = {
    e match {
      case Bencoding.Dict(dict) =>
        dict.get("t") match {
          case Some(Bencoding.Bytes(tid)) =>
            dict.get("y") match {
              case Some(Bencoding.Bytes("q")) =>
                (dict.get("q"), dict.get("a")) match {
                  case (Some(Bencoding.Bytes(method)),
                        Some(Bencoding.Dict(args))) => query(tid, method, args)
                  case _                            => None
                }
              case Some(Bencoding.Bytes("r")) =>
                dict.get("r") match {
                  case Some(Bencoding.Dict(vals)) => response(tid, vals)
                  case _                          => None
                }
              case Some(Bencoding.Bytes("e")) =>
                dict.get("e") match {
                  case Some(Bencoding.List(msg)) => error(tid, msg)
                  case _                         => None
                }
              case _ =>
                println("invalid dht packet (missing/invalid type")
                None
            }
          case _ =>
            println("invalid dht packet (missing/invalid transaction id")
            None
        }
      case _ =>
        println("invalid dht packet (not a dictionary)")
        println(e)
        None
    }
  }

  def query(tid : String, method : String, args : Map[String,Bencoding.Entry]) : Option[Bencoding.Dict] = {
    method match {
      case "ping"          => query_ping(tid, args)
      case "find_node"     => query_find_node(tid, args)
      case "get_peers"     => query_get_peers(tid, args)
      case "announce_peer" => query_announce_peer(args)
      case _               => println(method.pimped)
                              None
    }
  }

  def query_ping(tid : String, args : Map[String,Bencoding.Entry]) : Option[Bencoding.Dict] = {
    println("ping")
    args.get("id") match {
      case Some(Bencoding.Bytes(id)) =>
        //java.net.URLEncoder.encode(id, "ISO-8859-1") + ")")
        Option(Bencoding.Dict(Map("y" -> Bencoding.Bytes("r"),
                                  "t" -> Bencoding.Bytes(tid),
                                  "r" -> Bencoding.Dict(
          Map("id" -> Bencoding.Bytes(new String(DHT.my_id)))))))
      case _ => None
    }
  }

  def query_find_node(tid : String, args : Map[String,Bencoding.Entry]) : Option[Bencoding.Dict] = {
    println("find_node")
    (args.get("id"), args.get("target")) match {
      case (Some(Bencoding.Bytes(id)),
            Some(Bencoding.Bytes(target))) =>
        Option(Bencoding.Dict(Map("y" -> Bencoding.Bytes("r"),
                                  "t" -> Bencoding.Bytes(tid),
                                  "r" -> Bencoding.Dict(
          Map("id"    -> Bencoding.Bytes(new String(DHT.my_id)),
              "nodes" -> Bencoding.Bytes(""))))))
      case _ => ???
    }
  }

  def query_get_peers(tid : String, args : Map[String,Bencoding.Entry]) : Option[Bencoding.Dict] = {
    println("get_peers")
    (args.get("id"), args.get("info_hash")) match {
      case (Some(Bencoding.Bytes(id)),
            Some(Bencoding.Bytes(info_hash))) =>
        Option(Bencoding.Dict(Map("y" -> Bencoding.Bytes("r"),
                                  "t" -> Bencoding.Bytes(tid),
                                  "r" -> Bencoding.Dict(
          Map("id"     -> Bencoding.Bytes(new String(DHT.my_id)),
              "token"  -> Bencoding.Bytes("wtf"),
              //"values" -> Bencoding.Bytes("values"), //values OR nodes
              "nodes" -> Bencoding.Bytes(""))))))
      case _ => ???
    }
  }

  def query_announce_peer(args : Map[String,Bencoding.Entry]) : Option[Bencoding.Dict] = {
    println("announce_peer")
    args.get("implied_port") match {
      case Some(Bencoding.Int(n)) if n!=0 =>
        // ignore port argument and use source of packet instead
        ???
      case _ =>
        // use port argument
        ???
    }
    (args.get("id"), args.get("info_hash"), args.get("token")) match {
      case (Some(Bencoding.Bytes(id)),
            Some(Bencoding.Bytes(info_hash)),
            Some(Bencoding.Bytes(token))) =>
        Option(Bencoding.Dict(Map("id" -> Bencoding.Bytes(new String(DHT.my_id))
                                )))
      case _ => None
    }
  }

  def response(tid : String, vals : Map[String,Bencoding.Entry]) = {
    println("parse_respone")
    ???
  }

  def error(tid : String, list : List[Any]) = {
    println("error: " + list)
    None
  }
}

object pimpme {
  implicit class pimped(val s : String) extends AnyVal {
    def pimped = "*" + s + "*"
  }
}


class Bucket (from : BigInt, to : BigInt) {
  var last_changed = 0
  // 2^160 -> 1 << 160
  // 2^159 -> 1 << 159
  

  def insert() = {
    ???
  }
  def is_full() = {
    ???
  }
}

class Node{
  var last_seen = 0
}

