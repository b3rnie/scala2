package bittorrent

import akka.actor.Actor
import akka.actor.ActorSystem

import akka.actor.Props
import akka.io.Udp

object Main {
  def main(args: Array[String]) {
    val system = ActorSystem("ServerSystem")
    val dhtServerActor = system.actorOf(Props(new DHTServer()), name = "dhtserver")

    println("sleeping..")
    Thread.sleep(20000L)
    println("sleep done..")
    dhtServerActor ! Udp.Unbind
  }
}
