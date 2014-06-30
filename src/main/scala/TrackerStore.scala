package bittorrent

import scala.collection.concurrent.TrieMap

object TrackerStore {
  // peer_id -> infohashes
//  val peers = new TrieMap[String, List()]

  val v = new TrieMap[String, String]
  // infohash -> stats
  val stats = new TrieMap[String,String]

  def snapshot() = {
    ???
    v.readOnlySnapshot
  }

  def remove(peer : String) = {
    ???
  }

  def insert(peer : String, info_hash : String) = {
    // insert into peers
    // 
    ???
  }
}
