package bittorrent

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{HashSet, SynchronizedSet}

object TrackerStore {
  val peers        = new TrieMap[String,Any]
  val torrentPeers = new TrieMap[String,HashSet[PeerData]]

  // peer_id -> infohashes
//  val peers = new TrieMap[String, List()]

  val v = new TrieMap[String, String]
  // infohash -> stats
  val stats = new TrieMap[String,String]

  def snapshot() = {
    ???
    v.readOnlySnapshot
  }

  def insert(req : Tracker.AnnounceRequest) = {
    peers.put(req.peerId + req.infoHash, req) match {
      case Some(_) =>
      case None =>
    }
    insertTorrentPeers(req.infoHash, req.ip, req.port)
  }

  def update(req : Tracker.AnnounceRequest) = {
    peers.put(req.peerId + req.infoHash, req) match {
      case Some(_) =>
      case None =>
    }
    insertTorrentPeers(req.infoHash, req.ip, req.port)
  }

  def done(req : Tracker.AnnounceRequest) = {
    update(req)
  }

  def remove(req : Tracker.AnnounceRequest) = {
    peers.remove(req.peerId + req.infoHash) match {
      case Some(_) => true
      case None    => false
    }
  }

  /*
   * Can become inconsistent with peers!
   */
  def insertTorrentPeers(infoHash : String,
                         ip       : String,
                         port     : Int) : Boolean = {
    torrentPeers.get(infoHash) match {
      case Some(set) =>
        set += PeerData(ip, port)
        true
      case None =>
        val set = new HashSet[PeerData] with SynchronizedSet[PeerData]
        set += PeerData(ip, port)
        torrentPeers.putIfAbsent(infoHash, set) match {
          case Some(_) => insertTorrentPeers(infoHash, ip, port)
          case None    => true
        }
    }
  }

  def removeTorrentPeers(infoHash : String,
                         ip       : String,
                         port     : Int) = {
    torrentPeers.get(infoHash) match {
      case Some(set) =>
        set -= PeerData(ip, port)
      case None =>
        None
    }
  }
}

case class PeerData(
  ip       : String,
  port     : Int
)

