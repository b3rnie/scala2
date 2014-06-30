package bittorrent

object Main {
  def main(args : Array[String]) {
    val dhtServer  = new DhtServer(6881)
    val httpTracker = new HttpTracker(8080)
    // val udpTracker  = new UdpTracker(port)
    println("sleeping..")
    Thread.sleep(60000L)
    println("stopping dht server")
    dhtServer.stop
    println("stopping http server")
    httpTracker.stop
  }
}
