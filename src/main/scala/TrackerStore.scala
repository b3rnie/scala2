package tracker

import akka.actor.{Actor, ActorSystem, Props}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{HashSet, SynchronizedSet}
import scala.concurrent.duration._
import scala.util.Random

object TrackerStore {
  case class PeerData (
    uploaded   : Long,
    downloaded : Long,
    left       : Long,
    time       : Long = System.currentTimeMillis
  )

  case class PeerAddress (
    id       : String,
    ip       : String,
    port     : Int,
    time     : Long = System.currentTimeMillis) {

    override def equals(o: Any) = o match {
      case that: PeerAddress =>
        this.id.equals(that.id) &&
        this.ip.equals(that.ip) &&
        this.port == that.port
      case _ => false
    }

    override def hashCode = id.hashCode ^ ip.hashCode ^ port.hashCode
  }

  val peers      = new TrieMap[String, PeerData]
  val torrents   = new TrieMap[String, HashSet[PeerAddress]]
  val stats      = new TrieMap[String, TorrentStats]

  val system     = ActorSystem("storage")
  val statsActor = system.actorOf(Props(new StatsActor()), name = "stats")
  //val pruneActor = system.actorOf(Props(new PruneActor()), name = "prune")
  //val storeActor = system.actorOf(Props(new StoreActor()), name = "store")


  class StatsActor extends Actor {
    import system.dispatcher
    override def preStart() = {
      system.scheduler.scheduleOnce(30.seconds, self, 'tick)
    }

    // override postRestart so we don't call preStart and schedule a new message
    override def postRestart(reason: Throwable) = {}

    def receive = {
      case 'tick =>
        val newStats = new TrieMap[String, TorrentStats]
        val updateStats = (infoHash : String, state : AnyRef) => {
          val s =
            newStats.get(infoHash) match {
              case Some(TorrentStats(c,i)) =>
                if(state == 'complete) TorrentStats(c+1, i)
                else                   TorrentStats(c,   i+1)
              case None =>
                if(state == 'complete) TorrentStats(1, 0)
                else                   TorrentStats(0, 1)
            }
          newStats.put(infoHash, s)
        }
        peers.iterator.foreach(e => {
          val peerId   = e._1.substring(0,  20)
          val infoHash = e._1.substring(20, 40)
          e._2 match {
            case PeerData(uploaded, downloaded, 0,    _) =>
              updateStats(infoHash, 'complete)
            case PeerData(uploaded, downloaded, left, _) =>
              updateStats(infoHash, 'incomplete)
          }
        })

        stats.keysIterator.foreach(k => {
          if(!newStats.contains(k))
            stats.remove(k)
        })
        stats ++= newStats
        system.scheduler.scheduleOnce(30.seconds, self, 'tick)
    }
  }

  def stop() = {
    system.shutdown
  }

  def snapshot() = {
    ???
    peers.readOnlySnapshot
  }

  def insert(req : AnnounceRequest) = {
    val peerData = PeerData(uploaded   = req.uploaded,
                            downloaded = req.downloaded,
                            left       = req.left
                          )
    peers.put(req.peerId + req.infoHash, peerData) match {
      case Some(_) =>
      case None    =>
    }
    insertTorrents(req.peerId, req.infoHash, req.ip, req.port)
  }

  def update(req : AnnounceRequest) = {
    insert(req)
  }

  def done(req : AnnounceRequest) = {
    insert(req)
  }

  def remove(req : AnnounceRequest) = {
    peers.remove(req.peerId + req.infoHash) match {
      case Some(_) => true
      case None    => false
    }
    removeTorrents(req.peerId, req.infoHash, req.ip, req.port)
  }

  def getPeers(infoHash : String,
               numwant  : Int) : List[Tuple3[String, String, Int]] = {
    torrents.get(infoHash) match {
      case Some(set) =>
        val all = set.toList.map(p => Tuple3(p.id, p.ip, p.port))
        Random.shuffle(all).take(numwant)
      case None =>
        List[Tuple3[String, String, Int]]()
    }
  }

  def getStats(infoHash : String) = {
    stats.get(infoHash) match {
      case Some(stat) =>
        println(stat)
        stat
      case None       =>
        println("no stat...")
        TorrentStats()
    }
  }

 /*
   * Can become inconsistent with peers!
   */
  def insertTorrents(peerId   : String,
                     infoHash : String,
                     ip       : String,
                     port     : Int) : Boolean = {
    torrents.get(infoHash) match {
      case Some(set) =>
        val peerAddress = PeerAddress(peerId, ip, port)
        set -= peerAddress
        set += peerAddress
        true
      case None =>
        val set = new HashSet[PeerAddress] with SynchronizedSet[PeerAddress]
        set += PeerAddress(peerId, ip, port)
        torrents.putIfAbsent(infoHash, set) match {
          case Some(_) => insertTorrents(peerId, infoHash, ip, port)
          case None    => false
        }
    }
  }

  def removeTorrents(peerId   : String,
                     infoHash : String,
                     ip       : String,
                     port     : Int) = {
    torrents.get(infoHash) match {
      case Some(set) =>
        set -= PeerAddress(peerId, ip, port)
      case None =>
        None
    }
  }
}
