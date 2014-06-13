package bittorrent

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.actor.Props

import akka.io._

import java.net._

object Main {
  def main(args: Array[String]) {
    val bytes = FileUtils.readFile("/home/bernie/testtorrent.torrent")
    var res = Bencoding.decode(bytes)
    println(res)
    var res2 = Bencoding.encode(res)
    println(res2.size)
    var s = new String(res2.toArray, "ISO-8859-1")
    FileUtils.writeFile("/home/bernie/testtorrent")(p => {
      p.write(s)
    })
   
    val system = ActorSystem("ServerSystem")
    val serverActor = system.actorOf(Props[UdpServer], name = "udpserver")
    println("sleeping..")
    Thread.sleep(2000L)
    println("sleep done..")
    serverActor ! Udp.Unbind
  }
}

class UdpServer extends Actor {
  import context.system
  IO(Udp) ! Udp.Bind(self, new InetSocketAddress("localhost", 6881))

  def receive = {
    case Udp.Bound(local) =>
      println(sender)
      println(local)
      println("bound!")
      context.become(ready(sender))
  }

  def ready(ref : ActorRef): Receive = {
    case Udp.Received(data, sender) =>
      val processed = ???// parse data etc., e.g. using PipelineStage
      //socket ! Udp.Send(data, remote) // example server echoes back
      //nextActor ! processed
    case Udp.Unbind  => ref ! Udp.Unbind
    case Udp.Unbound => //context.stop(self)
      context.system.shutdown()
  }
}

/*
class HelloActor extends Actor {
  println("ALIVE!!!")
  def receive = {
    case "hello" => println("hello back at you")
                    
    case _       => println("huh?")
  }
} */
