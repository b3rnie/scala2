package bittorrent

object Main {
  def main(args : Array[String]) {
    val dhtServer = new DHTServer(6881)
    val httpServer = new HttpServer()

    println("sleeping..")
    Thread.sleep(60000L)
    println("stopping dht server")
    dhtServer.stop
    println("stopping http server")
    httpServer.stop
  }
}
