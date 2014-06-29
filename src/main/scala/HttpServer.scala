package bittorrent

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Terminated}
import akka.io.{IO,Tcp}
import spray.can.Http
import spray.http._
import spray.routing.{HttpService, RequestContext}
import HttpMethods._
import java.net.InetSocketAddress

class HttpServer {
  implicit val system = ActorSystem("http")
  val ref             = system.actorOf(Props(new HttpServerActor()), name = "http_server")
  IO(Http) ! Http.Bind(ref, interface = "localhost", port = 8080)
  def stop = {
    ref ! Http.Unbind
  }
}

class HttpServerActor extends Actor with Logging {
  import context.system
  def receive: Receive = {
    case Http.Bound(address) =>
      info("bound to " + address)
    case Tcp.Bound =>
    case Http.Connected(remote, _) =>
      debug("Remote address {} connected" + remote)
      sender ! Http.Register(context.actorOf(Props(new ConnectionActor(remote, sender))))
    case Http.Unbound =>
      
    case Http.Unbind =>
      sender ! Http.Close
      context.system.shutdown()
    case other =>
      println("other msg: " + other)
  }
}

class ConnectionActor(remote : InetSocketAddress, connection : ActorRef) extends Actor with Logging {
  context.watch(connection)
  def receive: Receive = {
    case HttpRequest(GET, uri @ Uri.Path("/"), _, _, _) =>
      sender ! HttpResponse(status = 200, entity = "/")
    case HttpRequest(GET, Uri.Path("/ping"), _, _, _) =>
      sender ! HttpResponse(status = 200, entity = "pong")
    case HttpRequest(GET, Uri.Path("/scrape"), _, _, _) =>
      sender ! HttpResponse(status = 404, entity = "meh")
    case HttpRequest(GET, uri @ Uri.Path("/announce"), headers, _, _) =>
      /* So much work to avoid the UTF8 encoding in Uri/Uri.query
       * Must be a better way..
       */
      val hdr = headers.find(_ match {
        case HttpHeaders.`Raw-Request-URI`(x) => true
        case _                                => false
      })
      hdr match {
        case Some(HttpHeaders.`Raw-Request-URI`(str)) =>
          val rawuri = Uri(str,
                           java.nio.charset.Charset.forName("ISO8859-1"),
                           spray.http.Uri.ParsingMode.Relaxed)
          val rawquery = rawuri.query.toMap
          //val q = TrackerParams.parseQuery(rawquery)
          val res = Bencoding.encode(Bencoding.Dict(
            Map("failure reason" -> Bencoding.Bytes("We failed!"))))
          sender ! HttpResponse(entity = new String(res.toArray))
        case _ =>
          sender ! HttpResponse(status = 500, entity = "I cant configure software")
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
    case other =>
      println("OTHER MSG: " + other)
  }
}

sealed abstract class HttpTrackerRequest

case class HttpAnnounceRequest (
  // required
  infoHash   : String,
  peerId     : String,
  port       : Int,
  uploaded   : Long,
  downloaded : Long,
  left       : Long,
  // optional
  ip         : String, // ip/dns
  event      : AnyRef,
  numwant    : Int,
  noPeerId   : Boolean,
  compact    : Boolean
) extends HttpTrackerRequest

case class HttpScrapeRequest (

) extends HttpTrackerRequest

object TrackerParams {
  def p_info_hash(x : Option[String]) = {
    assert(x.get.length == 20)
    x.get
  }
  def p_peer_id(x : Option[String]) = {
    assert(x.get.length == 20)
    x.get
  }

  def p_port(x : Option[String]) = {
    val v= x.get.toInt
    assert(v > 0 && v <= 65535)
    v
  }

  def p_uploaded(x : Option[String]) = {
    val v = x.get.toLong
    assert(v >= 0)
    v
  }

  def p_downloaded(x : Option[String]) = {
    val v = x.get.toLong
    assert(v >= 0)
    v
  }

  def p_left(x : Option[String]) = {
    val v = x.get.toLong
    assert(v >= 0)
    v
  }

  def p_ip(x : Option[String]) = {
    x match {
      case Some(dnsOrIp) => dnsOrIp
      case None          => ""
    }
  }

  def p_event(x : Option[String]) = {
    x match {
      case Some("started")   => 'started
      case Some("completed") => 'completed
      case Some("stopped")   => 'stopped
      case _                 => 'keepalive
    }
  }

  def p_numwant(x : Option[String]) = {
    x match {
      case Some(str) =>
        val v = str.toInt
        assert(v >= 0)
        v
      case None      => 50
    }
  }

  def p_no_peer_id(x : Option[String]) = {
    x match {
      case(Some(_)) => true
      case _        => false
    }
  }

  def p_compact(x : Option[String]) = {
    x match {
      case Some("1") => true
      case _         => false
    }
  }

  def p_key(x : Option[String]) = {
    x match {
      case Some(str) => str
      case _         => ""
    }
  }

  def p_trackerid(x : Option[String]) = {
    x match {
      case Some(str) => str
      case _         => ""
    }
  }

  def parseQuery(q : Map[String,String]) : HttpAnnounceRequest = {
    HttpAnnounceRequest(
      infoHash   = p_info_hash(q.get("info_hash")),
      peerId     = p_peer_id(q.get("peer_id")),
      port       = p_port(q.get("port")),
      uploaded   = p_uploaded(q.get("uploaded")),
      downloaded = p_downloaded(q.get("downloaded")),
      left       = p_left(q.get("left")),
      ip         = p_ip(q.get("ip")),
      event      = p_event(q.get("event")),
      numwant    = p_numwant(q.get("numwant")),
      noPeerId   = p_no_peer_id(q.get("nopeerid")),
      compact    = p_compact(q.get("compact"))
    )
  }
  //case (None,     'required) => throw(new Exception("missing param " + p._1))
}
