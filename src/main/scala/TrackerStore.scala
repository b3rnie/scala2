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
  sealed trait PeerState
  case object Online extends PeerState
  case object Offline extends PeerState

  case class PeerData (
    uploaded   : Long,
    downloaded : Long,
    left       : Long,
    ip         : String,
    port       : Int,
    state      : PeerState,
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
                       port       = req.port,
                       state      = Online
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
    val k = Tuple2(req.peerId, req.infoHash)
    peers.get(k).foreach(peerdata => {
      peers.put(k, peerdata.copy(state = Offline,
                                 time  = System.currentTimeMillis
                               ))
    })
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

  /* Periodically update index, mark 'disappeared' nodes as offline */
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
      torrents.iterator.foreach {
        case (hash, set) =>
          set.filter(p => {
            p match {
              case PeerAddress(_, _, _, time) if isFresh(tsnow, time) => true
              case PeerAddress(_, _, _, _)                            => false
            }
          })
          if(set.size == 0) {
            torrents.remove(hash)
          }
      }
    }

    def updateIndex() = {
      val tsnow = System.currentTimeMillis
      peers.iterator.foreach {
        case (key,      PeerData(_,_,_,_,_,Offline,_   )) => None
        case (key, pd @ PeerData(_,_,_,_,_,Online, time)) if !isFresh(tsnow, time) =>
          peers.put(key, pd.copy(state = Offline))
        case ((peerid, infohash), PeerData(_,_,_,ip,port,_,_)) =>
          val peeraddress = PeerAddress(peerid, ip, port)
          torrents.get(infohash) match {
            case Some(set) =>
              // must be a better way..
              set -= peeraddress
              set += peeraddress
            case None =>
              val set = new HashSet[PeerAddress]
                        with SynchronizedSet[PeerAddress]
              set += peeraddress
              torrents.putIfAbsent(infohash, set).foreach(newset => {
                newset += peeraddress
              })
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
        val newstats = new TrieMap[Infohash, TorrentStats]
        peers.iterator.foreach {
          case ((peerid, infohash), PeerData(_,_,0,_,_,Offline,_)) =>
            newstats.get(infohash) match {
              case Some(TorrentStats(complete,downloaded,incomplete)) =>
                newstats.put(infohash, TorrentStats(complete,downloaded+1,incomplete))
              case None =>
                newstats.put(infohash, TorrentStats(0, 1, 0))
            }
          case ((peerid, infohash), PeerData(_,_,_,_,_,Offline,_)) => None
          case ((peerid, infohash), PeerData(_,_,0,_,_,Online,_ )) =>
            newstats.get(infohash) match {
              case Some(TorrentStats(complete,downloaded,incomplete)) =>
                newstats.put(infohash, TorrentStats(complete+1,downloaded+1,incomplete))
              case None =>
                newstats.put(infohash, TorrentStats(1, 1, 0))
            }
          case ((peerid, infohash), PeerData(_,_,_,_,_,Online,_)) =>
            newstats.get(infohash) match {
              case Some(TorrentStats(complete,downloaded,incomplete)) =>
                newstats.put(infohash, TorrentStats(complete, downloaded, incomplete + 1))
              case None =>
                newstats.put(infohash, TorrentStats(0, 0, 1))
            }
          }
        stats ++= newstats
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
