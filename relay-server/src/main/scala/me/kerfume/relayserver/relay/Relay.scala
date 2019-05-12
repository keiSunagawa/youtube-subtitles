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
import io.circe.generic.JsonCodec

trait Relay[F[_]] {
  import Relay.POSTParameters

  def post(params: POSTParameters): F[Json]
  def get(url: String): F[Json]
}

object Relay {
  def postRequest[F[_]: Applicative](params: POSTParameters)(implicit e1: EntityEncoder[F, String], e2: EntityEncoder[F, UrlForm]) = {
    val cookie = params.cookie.foldLeft("") { case (acm, next) =>
        acm ++ s"${next.key}=${next.value}; "
    }.init.init
    val cookieHeader = Header("cookie", cookie)
    val headers = cookieHeader +: params.headers.map(h => Header(h.key, h.value))
    val url = Uri.fromString(params.url).right.get // TODO safety
    params.body match {
      case RelayJson(json) =>
        (new WithBodyOps[F](Method.POST)).apply(json.toString, url, headers: _*)
      case RelayFormData(body) =>
        val form = body.map(b => b.key -> b.value)
        (new WithBodyOps[F](Method.POST)).apply(UrlForm(form: _*), url, headers: _*)
    }
  }

  def impl[F[_]: Applicative: ConcurrentEffect: Monad](C: Client[F]): Relay[F] = new Relay[F] {
    override def post(params: POSTParameters): F[Json] = {
      println(params)
      for {
        res <- C.expect[String](postRequest[F](params)).map { s =>
          parse(s).right.get
        }
      } yield res
    }
    override def get(url: String): F[Json] = {
      import io.circe.syntax._

      val ua = Header(
        "user-agent",
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.131 Safari/537.36"
      )
      val url2 = Uri.fromString(url).right.get // TODO safety
      val req = (new WithBodyOps[F](Method.GET)).apply(url2, ua)
      for {
        res <- C.fetch[Json](req){ res =>
          println(res.cookies.map(_.content))
          val vil = res.cookies.find(_.name == "VISITOR_INFO1_LIVE").get.content
          val ysc = res.cookies.find(_.name == "YSC").get.content
          implicitly[EntityDecoder[F, String]].decode(res,  strict = false).leftWiden[Throwable].rethrowT.map { s =>
            GETRes(Cookie(vil, ysc), s).asJson
          }
        }
      } yield {
        res
      }
    }
  }

  import io.circe._, io.circe.generic.semiauto._
  import io.circe.{ Decoder, HCursor, Json }

  // GET Response Adhoc
  case class GETRes(
    cookie: Cookie,
    body: String
  )
  case class Cookie(
    vil: String,
    ysc: String
  )
  implicit val encodeCk: Encoder[Cookie] = deriveEncoder[Cookie]
  implicit val encodeGR: Encoder[GETRes] = deriveEncoder[GETRes]

  case class GETParameters(
    url: String
  )

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
        tpe <- c.downField("tpe").as[String]
        res <- tpe match {
          case "form" => c.downField("body").as[List[RelayKV]].map(RelayFormData)
          case "json" => c.downField("body").as[Json].map(RelayJson)
          case _ => Left(DecodingFailure("Attempt to decode value on failed cursor", Nil))
        }
      } yield res
  }
  implicit val relayJsonDecoder: Decoder[RelayJson] = deriveDecoder[RelayJson]
  implicit val relayKVDecoder: Decoder[RelayKV] = deriveDecoder[RelayKV]
  implicit val postDecoder: Decoder[POSTParameters] = deriveDecoder[POSTParameters]
  implicit val getDecoder: Decoder[GETParameters] = deriveDecoder[GETParameters]
}
