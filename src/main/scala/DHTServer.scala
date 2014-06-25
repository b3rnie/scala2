package bittorrent

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.io._
import akka.util.ByteString
import java.net._

class UDPServer(port : Integer) {
  val system = ActorSystem("ServerSystem")
  val ref    = system.actorOf(Props(new UDPServerActor(this, port)), name = "udpserver")

  def packet(data : ByteString, sender : InetSocketAddress) = {
    println("unimplemented!")
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
  IO(Udp) ! Udp.Bind(self, new InetSocketAddress("0.0.0.0", port))
  def receive = {
    case Udp.Bound(local) =>
      println(sender)
      println(local)
      println("bound!")
      context.become(ready(sender))
  }

  def ready(ref : ActorRef) : Receive = {
    case Udp.Received(data, sender) => daddy.packet(data, sender)
    //val processed = ???
    //ref ! Udp.Send(data, sender)
    case Udp.Send(data, to, _) => ref ! Udp.Send(data, to)
    case Udp.Unbind  => ref ! Udp.Unbind
    case Udp.Unbound =>
      //context.stop(self)
      context.system.shutdown()
  }
}

class DHTServer(port : Integer) extends UDPServer(port) {
  override def packet(data : ByteString, sender : InetSocketAddress) = {
    try {
      var dec = Bencoding.decode(data.toList)
      DHTMessage.message(dec).foreach( resp => {
        val resp2 = Bencoding.encode(resp).toArray
        super.send(resp2, sender)
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
  }
}
