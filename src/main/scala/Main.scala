package tracker

object Main extends Logging {
  def main(args : Array[String]) {
    val trackerDht  = new DhtTracker(6881)
    val trackerHttp = new HttpTracker(8080)
    // val udpTracker  = new UdpTracker(port)
    println("sleeping..")
    Thread.sleep(60000L)
    trackerDht.stop
    trackerHttp.stop

    TrackerStore.stop
  }
}

