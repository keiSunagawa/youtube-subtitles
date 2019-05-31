module Kerfume.GetSubtitle(runAsync) where

import Effect.Aff
import Kerfume.Decoder
import Kerfume.Relay
import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Kerfume.Regex (parsePostFormParams)

-- run :: Effect (Fiber Unit)
-- run = launchAff $ do
--   p <- getParameter
--   case p of
--     Right p' -> do
--       a <- getSubtitle p'.c p'.b
--       case a of
--           Right st' -> logShow st'
--           Left e -> logShow e
--     Left e -> liftEffect $ logShow e
runAsync :: String -> Aff (Array String)
runAsync url = do
  p <- getParameter url
  case p of
    Right p' -> do
      a <- getSubtitle p'.c p'.b
      case a of
          Right st' -> pure st'
          Left e -> faile e
    Left e -> faile e
  where
   faile e = pure [show e]

getSubtitle :: RelayCookies -> RelayBody ->  Aff (Either String (Array String))
getSubtitle c b = do
  res <- postRelay postUrl c postHeaders b
  pure $ do
    str <- getBody res (\e -> "GET /api response failed to decode: " <> AX.printResponseFormatError e)
    lmap (\x -> "failed decode subtitile") (decodeSubtitle str)

getParameter :: String -> Aff (Either String {c :: RelayCookies, b :: RelayBody, origin :: String})
getParameter url = do
  res <- try $ getRelay url getCookie getHeaders
  pure do
    res' <- lmap (\e -> "GET phase failed. " <> show e) res
    str <- getBody res' (\e -> "GET /api response failed to decode: " <> AX.printResponseFormatError e)
    Parameters params <- lmap (\e -> "parse failed. " <> show e) (decodeParams str)
    b <- parsePostFormParams params.body
    pure $ { b: (postForm b.trkprm b.sstkn b.csn b.prm), c: (postCookie params.cookie.vil params.cookie.ysc), origin: params.body}

-- get parameters
getCookie = RelayCookies []
getHeaders = RelayHeaders [ {key: "user-agent", value: "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.131 Safari/537.36"} ]

-- post parameters
postCookie vil ysc = RelayCookies [ {key: "VISITOR_INFO1_LIVE", value: vil}
                                  , {key: "YSC", value: ysc}]
postForm trkprm sstkn csn prm = Form { body: [ {key: "sej", value: "{\"clickTrackingParams\":\"" <> trkprm <> "\",\"commandMetadata\":{\"webCommandMetadata\":{\"url\":\"/service_ajax\",\"sendPost\":true}},\"getTranscriptEndpoint\":{\"params\":\"" <> prm <> "\"}}"}
                             , {key: "csn", value: csn }
                             , {key: "session_token", value: sstkn }] }
postHeaders = RelayHeaders [ {key: "x-youtube-client-version", value: "2.20190509"}
                       , {key: "x-youtube-client-name", value: "1" }]
postUrl = "https://www.youtube.com/service_ajax?name=getTranscriptEndpoint"
