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
import scala.annotation.meta.param
import org.http4s.RequestCookie

trait Relay[F[_]] {
  def post(params: POSTParameters): F[Json]
  def get(params: GETParameters): F[Json]
}

object Relay {
  def impl[F[_]](C: Client[F])(implicit m: Monad[F], c: ConcurrentEffect[F]): Relay[F] = new Impl[F](C)(m, c)
}

class Impl[F[_]](C: Client[F])(implicit m: Monad[F], c: ConcurrentEffect[F]) extends Relay[F] {
  private[this] def makeHeaders(params: RelayParameters): Seq[Header] = params.headers.map(h => Header(h.key, h.value))
  private[this] def addCookies(req: Request[F], params: RelayParameters): Request[F] =  {
    val cookies = params.cookie.map(c => RequestCookie(name = c.key, content = c.value))
    cookies.foldLeft(req)((r, n) => r.addCookie(n))
  }
  private[this] def postRequest[B](params: POSTParameters, body: B)(implicit en: EntityEncoder[F, B]) = {
    val headers = makeHeaders(params)
    val url = Uri.fromString(params.url).right.get // TODO safety
    val req = (new WithBodyOps[F](Method.POST)).apply[B](body, url, headers: _*)
    req.map { addCookies(_, params) }
  }
  private[this] def getRequest[B](params: GETParameters) = {
    val headers = makeHeaders(params)
    val url = Uri.fromString(params.url).right.get // TODO safety
    val req = (new WithBodyOps[F](Method.GET)).apply(url, headers: _*)
    req.map { addCookies(_, params) }
  }

  override def post(params: POSTParameters): F[Json] = {
    val req = params.body match {
      case RelayJson(json) =>
        val reqBody = json.toString // TODO
        postRequest(params, reqBody)
      case RelayFormData(body) =>
        val form = body.map(b => b.key -> b.value)
        val reBody = UrlForm(form: _*)
        postRequest(params, reBody)
    }
    for {
      res <- C.expect[String](req).map { s =>
        parse(s).right.get
      }
    } yield res
  }
  override def get(params: GETParameters): F[Json] = {
    import io.circe.syntax._

    val req = getRequest(params)
    for {
      res <- C.fetch[Json](req){ res =>
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
