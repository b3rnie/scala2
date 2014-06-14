package bittorrent

import akka.actor.Actor
import akka.actor.ActorRef
import akka.io._

import java.net._

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
        DHTMessage.parse(dec)
      } catch {
        case e : Bencoding.Bexception => { }
        case e : Exception => {
          println("exception!")
        }
        case e : Error => {
          println("error")
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

  def parse(e : Bencoding.Bentry) {
    e match {
      case Bencoding.Bdict(dict) =>
        dict.get("y") match {
          case Some(Bencoding.Bbytes(Array('q'))) =>
            var Bencoding.Bbytes(method) = dict.get("q").get
            var Bencoding.Bdict(args)   = dict.get("a").get
            println(new String(method))
            println(args)
          case Some(Bencoding.Bbytes(Array('r'))) => ???
          case Some(Bencoding.Bbytes(Array('e'))) => ???
          case None                               => ???
        }
      case _ => ???
    }
  }
}
