package bittorrent

import akka.actor.Actor
import akka.actor.ActorSystem

import akka.actor.Props
import akka.io.Udp

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
    val dhtServerActor = system.actorOf(Props[DHTServer], name = "dhtserver")
    //println("sleeping..")
    Thread.sleep(10000L)
    //println("sleep done..")
    dhtServerActor ! Udp.Unbind
  }
}

