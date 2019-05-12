module Kerfume.Relay where

import Effect.Aff
import Prelude
import Simple.JSON

import Affjax as AX
import Affjax.RequestBody (string)
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Class.Console (log)
import Kerfume.FetchParameters (decodeP, dbg)

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

-- bodyEncode :: RelayBody -> String
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

getParamEncode url =
  writeJSON { url: url }
-- TODO try catch
postRelay :: RelayURL -> RelayCookies -> RelayHeaders -> RelayBody -> Aff Unit
postRelay url cookie header body = do
  let req = AX.defaultRequest { url = "http://localhost:8080/relay"
                              , method = Left POST
                              , content = Just (string (postParamEncode url cookie header body))
                              , responseFormat = ResponseFormat.json
                              }
  res <- AX.request req
  case res.body of
    Left err -> log $ "GET /api response failed to decode: " <> AX.printResponseFormatError err
    Right json -> log $ "GET /api response: " <> (dbg $ do
      fr <- parseJSON $ J.stringify json
      decodeP fr)

-- TODO try catch
-- getRelay :: RelayURL -> Aff Unit
getRelay url = do
  let req = AX.defaultRequest { url = "http://localhost:8080/relay/get"
                              , method = Left POST
                              , content = Just (string (getParamEncode url))
                              , responseFormat = ResponseFormat.string
                              }
  AX.request req
