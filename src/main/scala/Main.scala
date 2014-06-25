package bittorrent

object Main {
  def main(args : Array[String]) {
    val dhtServer = new DHTServer(6881)
    println("sleeping..")
    Thread.sleep(10000L)
    println("sleep done..")
    dhtServer.stop
  }
}
