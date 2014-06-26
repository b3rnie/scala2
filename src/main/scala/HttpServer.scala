package bittorrent

import scala.concurrent.duration._
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.{Actor, ActorSystem, Props}
import akka.io.{IO,Tcp}

import spray.can.Http
//import spray.util._
import spray.http._

import HttpMethods._
//import MediaTypes._

class HttpServer {
  implicit val system = ActorSystem("http")
  val ref             = system.actorOf(Props(new HttpServerActor()), name = "http_server")
  IO(Http) ! Http.Bind(ref, interface = "localhost", port = 8080)
  def stop = {
    ref ! Http.Unbind
  }
}

class HttpServerActor extends Actor with Logging {
  implicit val timeout: Timeout = 2.second // for the actor 'asks'
  import context.system

  def receive = {
    case Http.Bound(address) =>
      info("bound to " + address)
    case Http.Unbound =>
      info("unbound")
      //context.stop(self)
      context.system.shutdown()
    case _: Http.Connected =>
      // this actor
      sender ! Http.Register(self)
    case Http.PeerClosed =>
    case HttpRequest(GET, Uri.Path("/announce"), _, _, _) =>
      info("announce request")
    case HttpRequest(GET, Uri.Path("/ping"), _, _, _) =>
      sender ! HttpResponse(entity = "PONG!")
    case _: HttpRequest =>
      sender ! HttpResponse(status = 404, entity = "Unknown resource!")
    case Http.Unbind =>
      sender ! Http.Close
      context.system.shutdown()
    case Timedout(HttpRequest(method, uri, _, _, _)) =>
      warn("timeout")
      sender ! HttpResponse(
        status = 500,
        entity = "The " + method + " request to '" + uri + "' has timed out..."
      )
     case other =>
      warn("other msg " + other)
  }
}
