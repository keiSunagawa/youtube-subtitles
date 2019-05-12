package me.kerfume.relayserver

import cats.effect.{ ConcurrentEffect, ContextShift, Effect, ExitCode, IO, IOApp, Timer }
import cats.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import org.http4s.server.middleware.Logger
import fs2.Stream
import me.kerfume.relayserver.relay.Relay

import scala.concurrent.ExecutionContext.global

import org.http4s.server.middleware._
import scala.concurrent.duration._

object RelayserverServer {

  def stream[F[_]: ConcurrentEffect](implicit T: Timer[F], C: ContextShift[F]): Stream[F, Nothing] = {
    for {
      client <- BlazeClientBuilder[F](global).stream
      helloWorldAlg = HelloWorld.impl[F]
      jokeAlg = Jokes.impl[F](client)
      toYtb = Relay.impl[F](client)

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract a segments not checked
      // in the underlying routes.
      httpApp = (
        RelayserverRoutes.helloWorldRoutes[F](helloWorldAlg) <+>
        RelayserverRoutes.jokeRoutes[F](jokeAlg) <+>
        // RelayserverRoutes.relayGet[F](toYtb) <+>
          RelayserverRoutes.relayPost[F](toYtb) <+>
          RelayserverRoutes.relayGet[F](toYtb)
      ).orNotFound

      originConfig = CORSConfig(
        anyOrigin = false,
        allowedOrigins = Set("http://localhost"),
        allowCredentials = false,
        maxAge = 1.day.toSeconds)
      // With Middlewares in place
      finalHttpApp = CORS(Logger.httpApp(false, false)(httpApp), originConfig)

      exitCode <- BlazeServerBuilder[F]
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(finalHttpApp)
        .serve
    } yield exitCode
  }.drain
}
