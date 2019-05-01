package me.kerfume.relayserver.relay

import cats.{ Applicative, Monad }
import io.circe.{ Decoder, Encoder, Json }
import org.http4s.client._
import cats.implicits._
import cats.effect._
import org.http4s.multipart.{ Multipart, Part }
import org.http4s.{ Method, Uri, UrlForm }
import org.http4s.client.dsl.io._
import org.http4s.headers._
import org.http4s.MediaType
import org.http4s._
import org.http4s.client.dsl.Http4sClientDsl._
import io.circe.parser._

trait Relay[F[_]] {
  def post(): F[Json]
}

object Relay {
  def postRequest[F[_]: Applicative] = (new WithBodyOps[F](Method.POST)).apply(
    UrlForm(
      "sej" -> """{"clickTrackingParams":"CFsQzKsBGAAiEwiu5ZzMoPnhAhVxwUwCHUt8ATY=","commandMetadata":{"webCommandMetadata":{"url":"/service_ajax","sendPost":true}},"getTranscriptEndpoint":{"params":"CgttTEpZT0RvYno0NA%3D%3D"}}""",
      "csn" -> "WADJXPKhHouOgAOT8pbgAQ",
      "session_token" -> "QUFFLUhqbHhVUTViZW1OWTVBNkV1eXZlN0trUm14dGtjQXxBQ3Jtc0tsQ212NUtzV0FTZHVSN0tnWEV0RmRXcE5HVkhFOWpLS3FiUF9nVzhIMWZMZUNFazNabHF2WnNOYkViOENqcDRrN3BkNVZrTzlZX0hJbHpxcFJwZ0NLR1dEYm5ZWjc0SHFVZWVrWGtYOWljZTZsUUxVQU5EUXBvMWRYYVhXQ3FZYzU0ZzE2SnJGRTNnV2UxYUxUOU9BOFJ0b01nTEE="
    ),
    Uri.uri("https://www.youtube.com/service_ajax?name=getTranscriptEndpoint"), // TODO remove deprecated
    Header("cookie", "VISITOR_INFO1_LIVE=zPSL3k2Nkcc; PREF=f1=50000000&f4=4000000; YSC=Pg1U1V1eYL0; GPS=1; ST-eeoeol=itct=CFsQzKsBGAAiEwjA8MaUnfXhAhVMP1gKHToMD70%3D&csn=3-PGXOirHcGo4wL5_ZjgCw"),
    Header("x-client-data", "CIu2yQEIpLbJAQjBtskBCKmdygEIqKPKAQixp8oBCOKoygEI8KnKAQivrMoBCLmsygEYl5jKAQ=="),
    Header("x-youtube-client-version", "2.20190423"),
    Header("x-youtube-client-name", "1"),
  )
  def impl[F[_]: Applicative: ConcurrentEffect: Monad](C: Client[F]): Relay[F] = new Relay[F] {
    override def post(): F[Json] = {
      for {
        res <- C.expect[String](postRequest[F]).map { s =>
          parse(s).right.get
        }
      } yield res
    }
  }

  import io.circe._, io.circe.generic.semiauto._
  import io.circe.{ Decoder, HCursor, Json }

  sealed trait RelayBody
  case class RelayFormData(body: Seq[RelayKV]) extends RelayBody
  case class RelayJson(body: Json) extends RelayBody
  case class RelayKV(key: String, value: String)
  case class POSTParameters(
    url: String,
    headers: Seq[RelayKV],
    cookie: Seq[RelayKV],
    body: RelayBody
  )
  implicit val relayFromDataDecoder: Decoder[RelayFormData] = deriveDecoder[RelayFormData]
  implicit val relayBodyDecoder: Decoder[RelayBody] = new Decoder[RelayBody] {
    final def apply(c: HCursor): Decoder.Result[RelayBody] =
      for {
        tpe <- c.downField("type").as[String]
        res <- tpe match {
          case "form" => c.downField("data").as[RelayFormData]
          case "json" => c.downField("data").as[Json].map(RelayJson)
          case _ => Left(DecodingFailure("Attempt to decode value on failed cursor", Nil))
        }
      } yield res
  }
  implicit val relayJsonDecoder: Decoder[RelayJson] = deriveDecoder[RelayJson]
  implicit val relayKVDecoder: Decoder[RelayKV] = deriveDecoder[RelayKV]
  implicit val postDecoder: Decoder[POSTParameters] = deriveDecoder[POSTParameters]
}
