package bittorrent

object Tracker {
  sealed abstract class Request
  case class AnnounceRequest (

    // required
    infoHash   : String,
    peerId     : String,
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
  ) extends Request

  sealed abstract class Reply
  case class AnnounceReplyOk (
    interval   : Int,
    trackerId  : String,
    complete   : Int,
    incomplete : Int,
    peers      : List[Tuple3[String,String,Int]]
  ) extends Reply

  case class AnnounceReplyError (
    failureReason : String
  ) extends Reply

  def handleRequest(req : AnnounceRequest) : Reply = {
    req.event match {
      case 'started   => TrackerStore.insert(req)
      case 'keepalive => TrackerStore.update(req)
      case 'completed => TrackerStore.done(req)
      case 'stopped   => TrackerStore.remove(req)
    }
    AnnounceReplyError(
      failureReason = "sod off!"
    )
  }

  def handleRequest(req : ScrapeRequest) : Reply = {
    ???
  }
}
