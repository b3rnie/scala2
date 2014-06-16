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
        DHTMessage.message(dec) match {
          case Some(resp) =>
            println(resp.size)
            val resp2 = ByteString(new String(Bencoding.encode(resp).toArray))
            ref ! Udp.Send(resp2, sender)
          case None =>
        }
      } catch {
        case e : Bencoding.DecodeException => { }
        case e : Exception => {
          println("exception!")
        }
        case e : Error => {
          println("error" + e)
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

  def message(e : Bencoding.Entry) : Option[List[Byte]] = {
     e match {
      case Bencoding.Dict(dict) =>
        dict.get("y") match {
          case Some(Bencoding.Bytes("q")) =>
            (dict.get("q"), dict.get("a")) match {
              case (Some(Bencoding.Bytes(method)),
                    Some(Bencoding.Dict(args))) => query(method, args)
              case _                            => None
            }
          case Some(Bencoding.Bytes("r"))) =>
            dict.get("r") match {
              case Some(Bencoding.Dict(vals)) => response(vals)
              case _                          => None
            }
          case Some(Bencoding.Bytes("e"))) =>
            dict.get("e") match {
              case Some(Bencoding.List(error)) => error(error)
              case _                           => None
            }
          case _ => None
        }
      case _ => None
    }
  }

  def query(method : String, args : Map[String,Bencoding.Entry]) : Option[List[Byte]] = {
    method match {
      case "ping"          => query_ping(args)
      case "find_node"     => query_find_node(args)
      case "get_peers"     => query_get_peers(args)
      case "announce_peer" => query_announce_peer(args)
      case _               => println(s.pimped)
                              None
    }
  }

  def query_ping(args : Map[String,Bencoding.Entry]) : Option[Bencoding.Dict] = {
    args.get("id").map(Bencoding.Bytes(id) =>
      Option(Bencoding.Dict(
        Map("id" -> Bencoding.Bytes("myid"))))
    )
  }

  def query_find_node(args : Map[String,Bencoding.Entry]) : Option[Bencoding.Dict] = {
    match (args.get("id"), args.get("target")) {
      case (Some(Bencoding.Bytes(id)),
            Some(Bencoding.Bytes(target))) =>
        Option(Bencoding.Dict(
          Map("id" -> Bencoding.Bytes("myid"),
              "nodes" -> Bencoding.Bytes("blah"))))
      case _ => ???
    }
  }

  def query_get_peers(args : Map[String,Bencoding.Entry]) : Option[Bencoding.Dict] = {
    match (args.get("id"), args.get("info_hash")) {
      case (Some(Bencoding.Bytes(id)),
            Some(Bencoding.Bytes(info_hash))) =>
        Option(Bencoding.Dict(
          Map("id" -> Bencoding.Bytes("myid"),
              "token" -> Bencoding.Bytes("wtf"),
              "values" -> Bencoding.Bytes("values"), //values OR nodes
              "nodes" -> Bencoding.Bytes("nodes"))))
      case _ => ???
    }
  }

  def query_announce_peer(args : Map[String,Bencoding.Entry]) : Option[Bencoding.Dict] = {
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
            Some(Bencoding.Bytes(token))) => ???
        Map("id" -> Bencoding.Bytes("myid"))))
      case None => None
  }

  def response(vals : Map[String,Bencoding.Entry]) = {
    println("parse_respone")
    ???
  }

  def error(list : List[Any]) = {
    println("error: " + list)
    ???
  }
}

object pimpme {
  implicit class pimped(val s : String) extends AnyVal {
    def pimped = "*" + s + "*"
  }
}
