package tracker

/* */
case class Infohash (
  hash : String) {
  require(hash.length() == 20, "Infohash is too short")
}

case class PeerId (
  hash : String) {
  require(hash.length() == 20, "PeerId is too short")
}

/* Requests */
sealed abstract class Request
case class AnnounceRequest (
  // required
  infoHash   : Infohash,
  peerId     : PeerId,
  port       : Int,
  uploaded   : Long,
  downloaded : Long,
  left       : Long,
  // optional
  ip         : String,
  event      : AnyRef,
  numwant    : Int,
  noPeerId   : Boolean,
  compact    : Boolean,
  key        : String,
  trackerId  : String
) extends Request

case class ScrapeRequest (
  hashes : List[Infohash]
) extends Request

/* Replies */
sealed abstract class Reply
case class AnnounceReplyOk (
  interval   : Int,
  trackerId  : String,
  complete   : Int,
  incomplete : Int,
  peers      : List[Tuple3[PeerId,String,Int]]
) extends Reply

case class AnnounceReplyError (
  failureReason : String
) extends Reply

case class ScrapeReplyOk (
  files : List[Tuple3[Long,Long,Long]] // FIXME: complete,downloaded,incomplete
) extends Reply

case class ScrapeReplyError (
) extends Reply


case class TorrentStats (
  complete   : Int = 0,
  incomplete : Int = 0
)

object Tracker {
  val announceInterval = 300
  val trackerId        = "foo"

  def handleRequest(req : AnnounceRequest) : Reply = {
    req.event match {
      case 'started   => TrackerStore.insert(req)
      case 'keepalive => TrackerStore.update(req)
      case 'completed => TrackerStore.done(req)
      case 'stopped   => TrackerStore.remove(req)
    }
    val peers = TrackerStore.getPeers(req.infoHash, req.numwant)
    val stats = TrackerStore.getStats(req.infoHash)
    AnnounceReplyOk(interval   = announceInterval,
                    trackerId  = trackerId,
                    complete   = stats.complete,
                    incomplete = stats.incomplete,
                    peers      = peers)
    //AnnounceReplyError(
    //  failureReason = "sod off!"
    //)
  }

  def handleRequest(req : ScrapeRequest) : Reply = {
    ???
  }
}
