package bittorrent

import akka.actor.Actor
import akka.actor.ActorSystem

import akka.actor.Props
import akka.io.Udp

import java.nio.ByteBuffer



object Main {
  def main(args: Array[String]) {
    val id = "my id"
    val system = ActorSystem("ServerSystem")
    val dhtServerActor = system.actorOf(Props(new DHTServer(id)), name = "dhtserver")
    println("sleeping..")
    Thread.sleep(60000L)
    println("sleep done..")
    dhtServerActor ! Udp.Unbind
  }
}

object DHT {
  //val md = java.security.MessageDigest.getInstance("SHA-1")
  def my_id = new Array[Byte](20)
  scala.util.Random.nextBytes(my_id)

  def distance(a : Int, b : Int) : Int = {
    a ^ b
  }

  def distance(a : Array[Byte], b : Array[Byte]) : Int = {
    ByteBuffer.wrap(a.zip(b).map(e => (e._1 ^ e._2).toByte)).getInt
  }
}
