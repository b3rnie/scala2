package bittorrent

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{HashSet, SynchronizedSet}
import scala.util.Random

object TrackerStore {
  val peers    = new TrieMap[String, PeerData]
  val torrents = new TrieMap[String, HashSet[PeerAddress]]

  def snapshot() = {
    ???
    peers.readOnlySnapshot
  }

  def insert(req : Tracker.AnnounceRequest) = {
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

  def update(req : Tracker.AnnounceRequest) = {
    insert(req)
  }

  def done(req : Tracker.AnnounceRequest) = {
    insert(req)
  }

  def remove(req : Tracker.AnnounceRequest) = {
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
          case None    => true
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
