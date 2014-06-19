package bittorrent

import akka.actor.Actor
import akka.actor.ActorRef
import akka.io._

import akka.util.ByteString

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
        DHTMessage.message(dec).foreach( resp => {
          val resp2 = ByteString(new String(Bencoding.encode(resp).toArray))
          ref ! Udp.Send(resp2, sender)
        })
      } catch {
        case e : Bencoding.DecodeException => {
          println("invalid incoming packet")
        }
        case e : Exception => {
          FileUtils.writeFile("/home/bernie/pkg")(p => {
            p.write(new String(data.toArray, "ISO-8859-1"))
          })
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
