package bittorrent

import scala.concurrent.duration._
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.{Actor, ActorRef, ActorSystem, Props, Terminated}
import akka.io.{IO,Tcp}

import spray.routing.{HttpService, RequestContext}

import spray.can.Http
//import spray.util._
import spray.http._

import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.{MalformedContent, FromStringDeserializer}

import HttpMethods._
//import MediaTypes._

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
          info("query: " + rawquery.get("info_hash").get.size)
          sender ! HttpResponse(entity = "announce")
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


object TrackerParams {
  val params = List(("info_hash", 'required, { }),
                    ("peer_id",   'required, {}),
                    ("port",      'required, {}),
                    ("uploaded",  'required, {}),
                    ("downloaded",'required, {}),
                    ("left",      'required, {}),
                    ("ip",        'optional, {}),
                    ("event",     'optional, {}),
                    ("numwant",   'optional, {}),
                    ("no_peer_id", 'optional, {}),
                    ("compact",    'optional, {})
                  )

  def parseQuery(query : Map[String,String]) : Map[String, Any] = {
    
  }


/*
                   "peer_id".as[HexString],
                   "port".as[Int],
                   "uploaded".as[Int],
                   "downloaded".as[Int],
                   "left".as[Int],
                   // optional
                   "ip".?,
                   "event" ? "keepalive",
                   // extensions
                   "numwant".as[Int] ? 50,
                   "no_peer_id".?,
                   "compact".?) {
          (info_hash, peer_id, port, uploaded, downloaded, left,
           ip, event, numwant, no_peer_id, compact) =>
          println("info hash: " + info_hash)
          println("peer id: " + peer_id)
          //println(ip)
          //println(port)
          complete("ok!")
        } */
