package bittorrent

import akka.actor.Actor
import akka.actor.ActorRef
import akka.io._

import akka.util.ByteString

import java.net._
import pimpme._

class DHTServer extends Actor {
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
        DHTMessage.parse(dec) match {
          case Some(resp) =>
            println(resp.size)
            println(ByteString("123"))
            val resp2 = ByteString(new String(resp.toArray))
            ref ! Udp.Send(resp2, sender)
          case None =>
            ???
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

  def parse(e : Bencoding.Entry) : Option[List[Byte]] = {
     e match {
      case Bencoding.Dict(dict) =>
        dict.get("y") match {
          case Some(Bencoding.Bytes(Array('q'))) =>
            (dict.get("q"), dict.get("a")) match {
              case (Some(Bencoding.Bytes(method)),
                    Some(Bencoding.Dict(args))) =>
                parse_query(method, args)
              case _ =>
                None
            }
          case Some(Bencoding.Bytes(Array('r'))) =>
            dict.get("r") match {
              case Some(Bencoding.Dict(vals)) =>
                parse_response(vals)
              case _ =>
                None
            }
          case Some(Bencoding.Bytes(Array('e'))) =>
            dict.get("e") match {
              case Some(Bencoding.List(error)) =>
                parse_error(error)
              case _ =>
                None
            }
          case _ => None
        }
      case _ => None
    }
  }

  def parse_query(method : Array[Byte], args : Map[String,Bencoding.Entry]) : Option[List[Byte]] = {
    var s = new String(method)
    s match {
      case "ping" =>
        var resp = Bencoding.encode(Bencoding.Dict(Map("id" -> Bencoding.Bytes("foobar".getBytes()))))
        println(resp)
        Option(resp)
      case "find_node" =>
        println("find node")
        ???
      case "get_peers" =>
        println("get_peers")
        ???
      case "announce_peer" =>
        println("announce_peer")
        ???
      case _ =>
        println(s.pimped)
        ???
    }
 }

  def parse_response(vals : Map[String,Bencoding.Entry]) = {
    println("parse_respone")
    ???
  }

  def parse_error(list : List[Any]) = {
    println("error: " + list)
    ???
  }
}

object pimpme {
  implicit class pimped(val s : String) extends AnyVal {
    def pimped = "*" + s + "*"
  }
}
