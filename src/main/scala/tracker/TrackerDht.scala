package tracker

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.io._
import akka.util.ByteString
import generic.Logging
import java.net._
import utils.{Bencoding, FileUtils}

abstract class UdpServerActor(port: Integer) extends Actor with Logging {
  import context.system
  IO(Udp) ! Udp.Bind(self, new InetSocketAddress("0.0.0.0", port))

  def receive = {
    case Udp.Bound(local) =>
      info("udp bound, sender: " + sender)
      info("udp bound, local: " + local)
      context.become(ready(sender))
    case Udp.Unbound =>
      context.stop(self)
      //context.system.shutdown()
    case other =>
      warn("unexpected message: " + other)
  }

  def ready(ref : ActorRef) : Receive = {
    case Udp.Received(data, sender) =>
      packet(ref, data, sender)
    //case Udp.Send(data, to, _) =>
    //  ref ! Udp.Send(data, to)
    case Udp.Unbind  =>
      ref ! Udp.Unbind
      context.unbecome
  }

  def packet(ref: ActorRef, data: ByteString, sender: InetSocketAddress): Unit
}

class DhtServerActor(port: Integer) extends UdpServerActor(port) with Logging {
  def packet(ref: ActorRef, data: ByteString, sender: InetSocketAddress) = {
    try {
      var dec = Bencoding.decode(data.iterator.buffered)
      DHTMessage.message(dec).foreach {
        case resp =>
          val resp2 = Bencoding.encode(resp)
          ref ! Udp.Send(ByteString(new String(resp2)), sender)
      }
    } catch {
      case e : Bencoding.DecodeException => {
        info("invalid incoming packet: " + e)
      }
      case e : Exception => {
        FileUtils.writeFile("/home/bernie/pkg")(p => {
          p.write(new String(data.toArray, "ISO-8859-1"))
        })
        warn("exception: " + e.getStackTrace)
      }
    }
  }
}

