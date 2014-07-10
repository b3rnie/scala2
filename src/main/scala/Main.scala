package tracker

object Main {
  def main(args : Array[String]) {
    val trackerDht = new DhtServer(6881)
    val trackerHttp = new HttpTracker(8080)
    // val udpTracker  = new UdpTracker(port)
    println("sleeping..")
    Thread.sleep(60000L)
    println("stopping dht server")
    trackerDht.stop
    println("stopping http server")
    trackerHttp.stop

    TrackerStore.stop
    val t = new Test[String]
  }
}

class Test[+A] {
  println("A)")
}
