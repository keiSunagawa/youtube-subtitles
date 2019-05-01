package me.kerfume.relayserver

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object Main extends IOApp {
  def run(args: List[String]) =
    RelayserverServer.stream[IO].compile.drain.as(ExitCode.Success)
}