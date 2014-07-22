package tracker

import java.io.{FileOutputStream,
                BufferedOutputStream,
                RandomAccessFile,
                File}

import akka.actor.{Actor, ActorSystem, Props}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{HashSet, SynchronizedSet}
import scala.concurrent.duration._
import scala.util.Random
import com.typesafe.config.ConfigFactory

object TrackerStore extends Logging {
  case class PeerData (
    uploaded   : Long,
    downloaded : Long,
    left       : Long,
    ip         : String,
    port       : Int,
    time       : Long = System.currentTimeMillis
  )
  case class PeerAddress (
    id       : PeerId,
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

  val peers       = new TrieMap[Tuple2[PeerId, Infohash], PeerData]
  val torrents    = new TrieMap[Infohash,                 HashSet[PeerAddress]]
  val stats       = new TrieMap[Infohash,                 TorrentStats]

  val system      = ActorSystem("store")
  val indexActor  = system.actorOf(Props(new IndexActor()), name = "index")
  val statsActor  = system.actorOf(Props(new StatsActor()), name = "stats")
  val storeActor  = system.actorOf(Props(new StoreActor()), name = "store")

  val trackerConf = ConfigFactory.load().getConfig("tracker")

  def stop() = {
    system.shutdown
  }

  def insert(req : AnnounceRequest) = {
    peers.put(Tuple2(req.peerId, req.infoHash),
              PeerData(uploaded   = req.uploaded,
                       downloaded = req.downloaded,
                       left       = req.left,
                       ip         = req.ip,
                       port       = req.port
                     )) match {
      case Some(_) =>
      case None    =>
    }
  }

  def update(req : AnnounceRequest) = {
    insert(req)
  }

  def done(req : AnnounceRequest) = {
    insert(req)
  }

  def remove(req : AnnounceRequest) = {
    // FIXME: dont delete only mark as offline or something
    peers.remove(Tuple2(req.peerId, req.infoHash)) match {
      case Some(_) => true
      case None    => false
    }
  }

  def getPeers(infohash : Infohash,
               numwant  : Int) : List[Tuple3[PeerId, String, Int]] = {
    torrents.get(infohash) match {
      case Some(set) =>
        val all = set.toList.map(p => Tuple3(p.id, p.ip, p.port))
        Random.shuffle(all).take(numwant)
      case None =>
        List[Tuple3[PeerId, String, Int]]()
    }
  }

  def getStats(infohash : Infohash) : TorrentStats = {
    stats.get(infohash) match {
      case Some(stats) => stats
      case None        => TorrentStats()
    }
  }

  /* Periodically update index */
  class IndexActor extends Actor {
    import system.dispatcher
    val TIMEOUT = 300000

    override def preStart() = {
      system.scheduler.scheduleOnce(5.seconds, self, 'tick)
    }
    override def postRestart(rsn : Throwable) = {}
    def receive = {
      case 'tick =>
        gcIndex()
        updateIndex()
        system.scheduler.scheduleOnce(5.seconds, self, 'tick)
    }

    def gcIndex() = {
      val tsnow = System.currentTimeMillis
      torrents.iterator.foreach(e => {
        val hash = e._1
        val set  = e._2
        set.filter(p => {
          p match {
            case PeerAddress(_, _, _, time) if isFresh(tsnow, time) => true
            case PeerAddress(_, _, _, _)                            => false
          }
        })
        if(set.size == 0) {
          torrents.remove(hash)
        }
      })
    }

    def updateIndex() = {
      val tsnow = System.currentTimeMillis
      peers.iterator.foreach {
        case (key, PeerData(_,_,_,_,_,time)) if !isFresh(tsnow, time) => None
        case ((peerId, infohash), PeerData(_,_,_,ip,port,_)) => {
          val peerAddress = PeerAddress(peerId, ip, port)
          torrents.get(infohash) match {
            case Some(set) =>
              // must be a better way..
              set -= peerAddress
              set += peerAddress
            case None =>
              val set = new HashSet[PeerAddress]
                        with SynchronizedSet[PeerAddress]
              set += peerAddress
              torrents.putIfAbsent(infohash, set) match {
                case Some(set) => set+= peerAddress
                case None      =>
              }
          }
        }
      }
    }

    def isFresh(tsnow : Long, tsthen : Long) : Boolean = {
      tsnow < tsthen + TIMEOUT
    }
  }

  /* Periodically calculate stats */
  class StatsActor extends Actor {
    import system.dispatcher
    override def preStart() = {
      system.scheduler.scheduleOnce(5.seconds, self, 'tick)
    }

    override def postRestart(rsn: Throwable) = {}

    def receive = {
      case 'tick =>
        val newStats = new TrieMap[Infohash, TorrentStats]
        peers.iterator.foreach {
          case ((peerId, infohash), PeerData(_,_,0,_,_,_)) => {
            newStats.get(infohash) match {
              case Some(TorrentStats(c,i)) => newStats.put(infohash,
                                                           TorrentStats(c+1, i))
              case None                    => newStats.put(infohash,
                                                           TorrentStats(1, 0))
            }
          }
          case ((peerId, infohash), PeerData(_,_,_,_,_,_)) => {
            newStats.get(infohash) match {
              case Some(TorrentStats(c,i)) => newStats.put(infohash,
                                                           TorrentStats(c, i+1))
              case None                    => newStats.put(infohash,
                                                           TorrentStats(0, 1))
            }
          }
        }
        stats ++= newStats
        system.scheduler.scheduleOnce(5.seconds, self, 'tick)
    }
  }

  /* Periodically dump db to disk */
  class StoreActor extends Actor {
    import system.dispatcher
    val STORE_ROOT = new File(trackerConf.getString("store"))
    val DB         = new File(STORE_ROOT, "db.bin")
    val DB_TMP     = new File(STORE_ROOT, "db.bin.tmp")
    override def preStart() = {
      STORE_ROOT.mkdirs()
      if(DB.exists()) {
        info("loading database..")
      } else {
        info("no database found")
      }
      system.scheduler.scheduleOnce(10.seconds, self, 'tick)
    }

    override def postRestart(rsn : Throwable) = {}
    def receive = {
      case 'tick =>
        val file   = new FileOutputStream(DB_TMP)
        val stream = new BufferedOutputStream(file)
        peers.iterator.foreach(e => {
          val buf = new Array[Byte](20)
          stream.write("ABCDEF0123456789".getBytes)
        })
        stream.close()
        file.close()
        DB_TMP.renameTo(DB)
        system.scheduler.scheduleOnce(10.seconds, self, 'tick)
    }
  }
}
