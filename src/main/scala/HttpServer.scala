package bittorrent

import scala.concurrent.duration._
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.{Actor, ActorSystem, Props}
import akka.io.{IO,Tcp}

import spray.routing.{HttpService, RequestContext}

import spray.can.Http
//import spray.util._
import spray.http._

import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.{MalformedContent, FromStringDeserializer}

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

class HttpServerActor extends Actor with HttpRoutes {
  // we use the enclosing ActorContext's or ActorSystem's dispatcher
  // for our Futures and Scheduler
  implicit def executionContext = actorRefFactory.dispatcher

  //implicit val timeout: Timeout = 2.second // for the actor 'asks'
  //import context.system

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing,
  // timeout handling or alternative handler registration
  def receive = runRoute(routes) orElse internalRequest

  val internalRequest: Receive = {
    case Http.Unbind =>
      warn("unbind request")
      //context.stop(self)
      sender ! Http.Close
      context.system.shutdown()
  }
}

trait HttpRoutes extends HttpService with Logging {
  type HexString = String

  implicit object _StringToHexString extends FromStringDeserializer[HexString] {
    def apply(source: String) = {
      //Right(source.toUpperCase)
      Left(MalformedContent("Not a hexstring"))
    }
  }

  val routes = {
    get {
      pathSingleSlash {
        complete("/")
      } ~
      path("ping") {
        complete("pong")
      } ~
      path("announce") {
        parameters(//required
                   "info_hash".as[HexString],
                   "peer_id".as[HexString],
                   "port".as[Int],
                   "uploaded.as[Int]",
                   "downloaded.as[Int]",
                   "left.as[Int]",

                   "ip".?,
                   "event" ? "keepalive",
                   "numwant".as[Int] ? 50,
                   "no_peer_id".?,
                   "compact".?) {
          (info_hash, peer_id, port, uploaded, downloaded, left,
           ip, event, numwant, no_peer_id, compact) =>
          println(info_hash)
          //println(ip)
          //println(port)
          complete("ok!")
        }
      }
    }
  }
}


