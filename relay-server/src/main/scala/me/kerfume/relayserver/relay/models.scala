package me.kerfume.relayserver.relay

import io.circe._, io.circe.generic.semiauto._
import io.circe.{ Decoder, HCursor, Json }

trait RelayParameters {
  val url: String
  val headers: Seq[RelayKV]
  val cookie: Seq[RelayKV]
}

case class GETParameters(
  url: String,
  headers: Seq[RelayKV],
  cookie: Seq[RelayKV]
) extends RelayParameters
object GETParameters {
  implicit val getDecoder: Decoder[GETParameters] = deriveDecoder[GETParameters]
}
case class POSTParameters(
  url: String,
  headers: Seq[RelayKV],
  cookie: Seq[RelayKV],
  body: RelayBody
) extends RelayParameters
object POSTParameters {
  implicit val postDecoder: Decoder[POSTParameters] = deriveDecoder[POSTParameters]
}


// GET Response Adhoc
case class GETRes(
  cookie: Cookie,
  body: String
)

object GETRes {
  implicit val encodeGR: Encoder[GETRes] = deriveEncoder[GETRes]
}

case class Cookie(
  vil: String,
  ysc: String
)

object Cookie {
  implicit val encodeCk: Encoder[Cookie] = deriveEncoder[Cookie]
}

sealed trait RelayBody
object RelayBody {
  implicit val relayBodyDecoder: Decoder[RelayBody] = new Decoder[RelayBody] {
  final def apply(c: HCursor): Decoder.Result[RelayBody] =
    for {
      tpe <- c.downField("tpe").as[String]
      res <- tpe match {
        case "form" => c.downField("body").as[List[RelayKV]].map(RelayFormData.apply)
        case "json" => c.downField("body").as[Json].map(RelayJson.apply)
        case _ => Left(DecodingFailure("Attempt to decode value on failed cursor", Nil))
      }
    } yield res
}

}
case class RelayFormData(body: Seq[RelayKV]) extends RelayBody
object RelayFormData {
  implicit val relayFromDataDecoder: Decoder[RelayFormData] = deriveDecoder[RelayFormData]
}

case class RelayJson(body: Json) extends RelayBody
object RelayJson {
  implicit val relayJsonDecoder: Decoder[RelayJson] = deriveDecoder[RelayJson]
}
case class RelayKV(key: String, value: String)
object RelayKV {
  implicit val relayKVDecoder: Decoder[RelayKV] = deriveDecoder[RelayKV]
}
