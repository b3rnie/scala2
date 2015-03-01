package tracker
import akka.actor.{
  Actor,
  ActorSystem,
  AllForOneStrategy,
  Props}
import akka.actor.SupervisorStrategy._
import generic.Logging
import scala.concurrent.duration._

object Main extends Logging {
  val system = ActorSystem("Tracker")
  val supervisor = system.actorOf(Props(new Supervisor()), name = "Supervisor")

  class Supervisor extends Actor {
    val dhtServerActor = context.actorOf(Props(new DhtServerActor(6881)), name = "DhtServer")
    override val supervisorStrategy =
      AllForOneStrategy(maxNrOfRetries = 0, withinTimeRange = 1.seconds) {
        case e: Exception =>
          error("shutting down: " + e)
          Stop
      }

    def receive = { case msg => warn("unexpected message: " + msg) }
  }

  def main(args: Array[String]) {
    Thread.sleep(10000)
    system.shutdown()
  }
  
  /*
    val trackerDht  = new DhtTracker(6881)
    val trackerHttp = new HttpTracker(8080)
    // val udpTracker  = new UdpTracker(port)
    println("sleeping..")
    Thread.sleep(60000L)
    trackerDht.stop
    trackerHttp.stop

    TrackerStore.stop
  } */
}

