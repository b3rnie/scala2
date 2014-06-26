package bittorrent

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.io._
import akka.util.ByteString
import java.net._

class UDPServer(port : Integer) extends Logging {
  implicit val system = ActorSystem("ServerSystem")
  val ref    = system.actorOf(Props(new UDPServerActor(this, port)), name = "udpserver")
  

  def packet(data : ByteString, sender : InetSocketAddress) = {
    warn("unimplemented")
  }

  def send(data : Array[Byte], to : InetSocketAddress) = {
    ref ! Udp.Send(ByteString(new String(data)), to)
  }

  def stop = {
    ref ! Udp.Unbind
  }
}

class UDPServerActor(daddy : UDPServer, port : Integer) extends Actor {
  import context.system
  IO(Udp) ! Udp.Bind(self, new InetSocketAddress("0.0.0.0", 6881))
  def receive = {
    case Udp.Bound(local) =>
      println(sender)
      println(local)
      context.become(ready(sender))
    case Udp.Unbound =>
      //context.stop(self)
      context.system.shutdown()
    case other =>
      println(other)
  }

  def ready(ref : ActorRef) : Receive = {
    case Udp.Received(data, sender) => daddy.packet(data, sender)
    //val processed = ???
    //ref ! Udp.Send(data, sender)
    case Udp.Send(data, to, _) => ref ! Udp.Send(data, to)
    case Udp.Unbind  => ref ! Udp.Unbind
      context.unbecome
  }
}

class DHTServer(port : Integer) extends UDPServer(port) {
  // val logger = Logger(LoggerFactory.getLogger("name"))
//  logger.debug("foo")
  override def packet(data : ByteString, sender : InetSocketAddress) = {
    try {
      var dec = Bencoding.decode(data.toList)
      DHTMessage.message(dec).foreach( resp => {
        val resp2 = Bencoding.encode(resp).toArray
        super.send(resp2, sender)
      })
    } catch {
      case e : Bencoding.DecodeException => {
        info("invalid incoming packet")
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
  }
}
