package me.kerfume.relayserver

import cats.Monad
import cats.effect.Sync
import cats.implicits._
import io.circe.Json
import me.kerfume.relayserver.relay.Relay
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{ HttpRoutes, UrlForm }

object RelayserverRoutes {

  def jokeRoutes[F[_]: Sync](J: Jokes[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "joke" =>
        for {
          joke <- J.get
          resp <- Ok(joke)
        } yield resp
    }
  }

  def helloWorldRoutes[F[_]: Sync](H: HelloWorld[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "hello" / name =>
        for {
          greeting <- H.hello(HelloWorld.Name(name))
          resp <- Ok(greeting)
        } yield resp
    }
  }

  def relayGet[F[_]: Sync](H: Relay[F]): HttpRoutes[F] = {
    import Relay._
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case req @ POST -> Root / "relay" / "get" =>
        (for {
          params <- req.decodeJson[GETParameters]
          resp <- H.get(params.url) >>= (Ok(_))
        } yield resp).handleErrorWith { e =>
          println(e)
          Conflict("client error.")
        }
    }
  }
  def relayPost[F[_]: Sync](H: Relay[F]): HttpRoutes[F] = {
    import Relay._
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case req @ POST -> Root / "relay" =>
        (for {
          params <- req.decodeJson[POSTParameters]
          resp <- H.post(params) >>= (Ok(_))
        } yield resp).handleErrorWith { e =>
          println(e)
          Conflict("client error.")
        }
    }
  }
}
