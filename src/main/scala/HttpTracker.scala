package bittorrent

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Terminated}
import akka.io.{IO,Tcp}
import spray.can.Http
import spray.http._
import spray.routing.{HttpService, RequestContext}
import HttpMethods._
import java.net.InetSocketAddress

class HttpTracker(port : Int) {
  implicit val system = ActorSystem("http")
  val ref             = system.actorOf(Props(new HttpTrackerActor()), name = "http_tracker")
  IO(Http) ! Http.Bind(ref, interface = "localhost", port = port)
  def stop = {
    ref ! Http.Unbind
  }
}

class HttpTrackerActor extends Actor with Logging {
  import context.system
  def receive: Receive = {
    case Http.Bound(address) =>
      info("bound to " + address)
    case Tcp.Bound =>
    case Http.Connected(remote, _) =>
      debug("Remote address {} connected" + remote)
      sender ! Http.Register(context.actorOf(Props(new HttpTrackerConnectionActor(remote, sender))))
    case Http.Unbound =>
    case Http.Unbind =>
      sender ! Http.Close
      context.system.shutdown()
    case other =>
      warn("other msg: " + other)
  }
}

class HttpTrackerConnectionActor(
  remote : InetSocketAddress,
  connection : ActorRef) extends Actor with Logging {

  context.watch(connection)
  def receive: Receive = {
    case HttpRequest(GET, uri @ Uri.Path("/"), _, _, _) =>
      sender ! HttpResponse(status = 200, entity = "/")
    case HttpRequest(GET, Uri.Path("/ping"), _, _, _) =>
      sender ! HttpResponse(status = 200, entity = "pong")
    case HttpRequest(GET, Uri.Path("/scrape"), headers, _, _) =>
      try {
        val req = HttpTrackerRequest.parseScrapeRequest(remote,
                                                        getQuery(headers))
        val res = Tracker.handleRequest(req)
        val rep = HttpTrackerRequest.generateReply(req, res)
        sender ! HttpResponse(entity = rep)
      } catch {
        case e : HttpTrackerRequest.ParseException =>
          sender ! HttpResponse(status = 400, entity = "You fucked up")
      }
    case HttpRequest(GET, Uri.Path("/announce"), headers, _, _) =>
      try {
        val req = HttpTrackerRequest.parseAnnounceRequest(remote,
                                                          getQuery(headers))
        val res = Tracker.handleRequest(req)
        val rep = HttpTrackerRequest.generateReply(req, res)
        sender ! HttpResponse(entity = rep)
        // new String(Bencoding.encode(resp).toArray))
      } catch {
        case e : HttpTrackerRequest.ParseException => {
          val resp = Bencoding.encode(Bencoding.Dict(
            Map("failure reason" -> Bencoding.Bytes(e.getMessage))))
          sender ! HttpResponse(entity = new String(resp.toArray))
        }
        case _ : Exception =>
          sender ! HttpResponse(status = 500, entity = "Internal Error")
      }
    case _: HttpRequest =>
      sender ! HttpResponse(status = 404, entity = "sod off")
    case _: Tcp.ConnectionClosed =>
      info("Stopping, because connection for remote address {} closed")
      //context.unwatch(connection)
      //context.stop(self)
    case Terminated(`connection`) =>
      info("Stopping, because connection for remote address {} died")
      context.unwatch(connection)
      context.stop(self)
    case Timedout(HttpRequest(method, uri, _, _, _)) =>
      warn("Request timed out")
      sender ! HttpResponse(
        status = 500,
        entity = "The " + method + " request to '" + uri + "' has timed out..."
      )
    case other =>
      warn("OTHER MSG: " + other)
  }

  def getQuery(headers : List[HttpHeader]) : Uri.Query = {
    /* So much work to avoid the UTF8 encoding in Uri/Uri.query
     * Must be a better way..
     */
    headers.find(_ match {
      case HttpHeaders.`Raw-Request-URI`(x) => true
      case _                                => false
    }) match {
      case Some(HttpHeaders.`Raw-Request-URI`(str)) =>
        val rawuri = Uri(str,
                         java.nio.charset.Charset.forName("ISO8859-1"),
                         spray.http.Uri.ParsingMode.Relaxed)
        rawuri.query
      case _ =>
        throw new Exception("misconfigured spray server")
    }
  }
}

object HttpTrackerRequest {
  case class ParseException(s : String) extends Exception {
    override def getMessage = s
  }

  def parse20Bytes(what : String, str : String) = {
    if(str.length != 20)
      throw new ParseException(what + " not 20 bytes")
    str
  }

  def parsePort(str : String) = {
    val v =
      try {
        str.toInt
      } catch {
        case _ : Exception => throw new ParseException("port not an integer")
      }
    if (v<0 || v>65535)
      throw new ParseException("port out of bounds")
    v
  }

  def parseLong(what : String, str : String) = {
    val v = try {
      str.toLong
    } catch {
      case _ : Exception => throw new ParseException(what + " not an integer")
    }
    if(v < 0)
      throw new ParseException(what + " is negative")
    v
  }

  def parseIp(x : Option[String], remote : InetSocketAddress) = {
    x match {
      case Some(dnsOrIp) => dnsOrIp // checkme!
      case None          => remote.getAddress.getHostAddress
    }
  }

  def parseEvent(x : Option[String]) = {
    x match {
      case Some("started")   => 'started
      case Some("completed") => 'completed
      case Some("stopped")   => 'stopped
      case _                 => 'keepalive
    }
  }

  def parseNumwant(x : Option[String]) = {
    x match {
      case Some(str) =>
        val v = str.toInt
        assert(v >= 0)
        v
      case None => 50
    }
  }

  def parseNoPeerId(x : Option[String]) = {
    x match {
      case(Some(_)) => true
      case _        => false
    }
  }

  def parseCompact(x : Option[String]) = {
    x match {
      case Some("1") => true
      case _         => false
    }
  }

  def parseKey(x : Option[String]) = {
    x match {
      case Some(str) => str
      case _         => ""
    }
  }

  def parseTrackerId(x : Option[String]) = {
    x match {
      case Some(str) => str
      case _         => ""
    }
  }

  def parseAnnounceRequest(remote : InetSocketAddress,
                           query : Uri.Query) : Tracker.AnnounceRequest = {
    val q = query.toMap
    Tracker.AnnounceRequest(
      // required
      infoHash   = parse20Bytes("info_hash", getRequired("info_hash", q)),
      peerId     = parse20Bytes("peer_id",   getRequired("peer_id", q)),
      port       = parsePort(                getRequired("port", q)),
      uploaded   = parseLong("uploaded",     getRequired("uploaded", q)),
      downloaded = parseLong("downloaded",   getRequired("downloaded", q)),
      left       = parseLong("left",         getRequired("left", q)),
      // optional
      ip         = parseIp(q.get("ip"), remote),
      event      = parseEvent(q.get("event")),
      numwant    = parseNumwant(q.get("numwant")),
      noPeerId   = parseNoPeerId(q.get("no_peer_id")),
      compact    = parseCompact(q.get("compact")),
      key        = parseKey(q.get("key")),
      trackerId  = parseTrackerId(q.get("trackerid"))
    )
  }
  
  def parseScrapeRequest(remote : InetSocketAddress,
                         query : Uri.Query) : Tracker.ScrapeRequest = {
    //query.getAll
    ???
  }

  def getRequired(field : String, q : Map[String,String]) : String = {
    q.get(field) match {
      case Some(str) => str
      case None      => throw(new ParseException("missing parameter " + field))
    }
  }

  

  def generateReply(req : Tracker.Request,
                    rep : Tracker.Reply) = {
    rep match {
      case rep : Tracker.AnnounceReplyError =>
        Bencoding.encode(Bencoding.Dict(
          Map("failure reason" -> Bencoding.Bytes(rep.failureReason))))
      case rep : Tracker.AnnounceReplyOk =>
        Bencoding.encode(
          Bencoding.Dict(
            Map("interval"        -> Bencoding.Int(rep.interval),
                //"warning message" -> Bencoding.Bytes(new String("warning here..")),
                "tracker id"      -> Bencoding.Bytes(rep.trackerId),
                "complete"        -> Bencoding.Int(rep.complete),
                "incomplete"      -> Bencoding.Int(rep.incomplete),
                "peers"           -> Bencoding.List(List()))))
    }
  }
}