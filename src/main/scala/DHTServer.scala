package bittorrent

import akka.actor.Actor
import akka.actor.ActorRef
import akka.io._

import akka.util.ByteString

import java.net._
import pimpme._

class DHTServer(id : String) extends Actor {
  import context.system
  IO(Udp) ! Udp.Bind(self, new InetSocketAddress("0.0.0.0", 6881))

  def receive = {
    case Udp.Bound(local) =>
      println(sender)
      println(local)
      println("bound!")
      context.become(ready(sender))
  }

  def ready(ref : ActorRef): Receive = {
    case Udp.Received(data, sender) =>
      try {
        var dec = Bencoding.decode(data.toList)
        DHTMessage.message(dec).foreach( resp => {
          val resp2 = ByteString(new String(Bencoding.encode(resp).toArray))
          ref ! Udp.Send(resp2, sender)
        })
      } catch {
        case e : Bencoding.DecodeException => { }
        case e : Exception => {
          println(new String(data.toArray))
          println("error" + e.getStackTrace)
        }
        case e : Error => {
          
        }
      }
    //val processed = ???
    //ref ! Udp.Send(data, sender)
    case Udp.Unbind  =>
      ref ! Udp.Unbind
    case Udp.Unbound =>
      //context.stop(self)
      context.system.shutdown()
  }
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
         (dict.get("y"), dict.get("t")) match {
          case (Some(Bencoding.Bytes("q")),
                Some(Bencoding.Bytes(tid))) =>
            (dict.get("q"), dict.get("a")) match {
              case (Some(Bencoding.Bytes(method)),
                    Some(Bencoding.Dict(args))) => query(tid, method, args)
              case _                            => None
            }
          case (Some(Bencoding.Bytes("r")),
                Some(Bencoding.Bytes(tid))) =>
            dict.get("r") match {
              case Some(Bencoding.Dict(vals)) => response(vals)
              case _                          => None
            }
          case (Some(Bencoding.Bytes("e")),
                Some(Bencoding.Bytes(tid))) =>
            dict.get("e") match {
              case Some(Bencoding.List(msg)) => error(msg)
              case _                           => None
            }
          case _ => None
        }
      case _ => None
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

  def response(vals : Map[String,Bencoding.Entry]) = {
    println("parse_respone")
    ???
  }

  def error(list : List[Any]) = {
    println("error: " + list)
    None
  }
}

object pimpme {
  implicit class pimped(val s : String) extends AnyVal {
    def pimped = "*" + s + "*"
  }
}
