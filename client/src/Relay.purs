module Kerfume.Relay where

import Effect.Aff
import Prelude
import Simple.JSON

import Affjax as AX
import Affjax.RequestBody (RequestBody(..), string)
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Class.Console (log)
import Kerfume.Decoder (dbg, decodeSubtitle)

-- definitions
type RelayURL = String
data RelayBody = Form { body :: Array RelayKV }
data RelayFormBody
type RelayKV = { key :: String, value :: String }
newtype RelayCookies = RelayCookies (Array RelayKV)
newtype RelayHeaders = RelayHeaders (Array RelayKV)
newtype RelayPOSTParameters a = RelayPOSTParameters
                              { url :: String
                              , headers :: Array RelayKV
                              , cookie :: Array RelayKV
                              , body :: a
                              }
type RelayResponse = (AX.Response (Either AX.ResponseFormatError String))

-- helpers
getBody :: RelayResponse -> (AX.ResponseFormatError -> String) -> Either String String
getBody res handle = lmap handle res.body

-- TODO try catch
postRelay :: RelayURL -> RelayCookies -> RelayHeaders -> RelayBody -> Aff RelayResponse
postRelay url cookie header body = do
  AX.request AX.defaultRequest { url = "http://localhost:8080/relay"
                              , method = Left POST
                              , content = Just (string (postParamEncode url cookie header body))
                              , responseFormat = ResponseFormat.string
                              }

-- TODO try catch
getRelay :: RelayURL -> RelayCookies -> RelayHeaders -> Aff RelayResponse
getRelay url cookie headers = do
  AX.request AX.defaultRequest { url = "http://localhost:8080/relay/get"
                              , method = Left POST
                              , content = Just (string (getParamEncode url cookie headers))
                              , responseFormat = ResponseFormat.string
                              }

bodyEncode :: RelayBody -> {tpe :: String, body :: Array RelayKV}
bodyEncode body = case body of
  Form { body: body' } ->
    { tpe: "form", body: body' }

postParamEncode :: RelayURL -> RelayCookies -> RelayHeaders -> RelayBody -> String
postParamEncode url (RelayCookies cookie) (RelayHeaders headers) body =
  writeJSON { url: url
            , headers: headers
            , cookie: cookie
            , body: bodyEncode(body)
            }

getParamEncode :: RelayURL -> RelayCookies -> RelayHeaders -> String
getParamEncode url (RelayCookies cookie) (RelayHeaders headers) =
  writeJSON { url: url
            , headers: headers
            , cookie: cookie
            }
