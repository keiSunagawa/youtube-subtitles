module Kerfume.Http where

import Effect.Aff
import Kerfume.FFI
import Kerfume.FetchParameters
import Kerfume.Relay
import Kerfume.TextProc
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

makeCookie vil ysc = RelayCookies [ {key: "VISITOR_INFO1_LIVE", value: vil}
                      , {key: "YSC", value: ysc}]
headers = RelayHeaders [ {key: "x-youtube-client-version", value: "2.20190509"}
                       , {key: "x-youtube-client-name", value: "1" }]
makeBody trkprm sstkn csn = Form { body: [ {key: "sej", value: "{\"clickTrackingParams\":\"" <> trkprm <> "\",\"commandMetadata\":{\"webCommandMetadata\":{\"url\":\"/service_ajax\",\"sendPost\":true}},\"getTranscriptEndpoint\":{\"params\":\"CgttTEpZT0RvYno0NA%3D%3D\"}}"}
                             , {key: "csn", value: csn }
                             , {key: "session_token", value: sstkn }] }
url = "https://www.youtube.com/service_ajax?name=getTranscriptEndpoint"

postTo :: RelayCookies -> RelayBody -> Aff Unit
postTo c b = do
  -- delay (Milliseconds 10000.0)
  postRelay url c headers b

getParameter :: Aff (Either String {c :: RelayCookies, b :: RelayBody, origin :: String})
getParameter = do
  res <- try $ getRelay "https://www.youtube.com/watch?v=mLJYODobz44&t=1s"
  pure do
    res' <- lmap (\e -> "GET phase failed. " <> show e) res
    str <- lmap (\e -> "GET /api response failed to decode: " <> AX.printResponseFormatError e) res'.body
    Parameters params <- lmap (\e -> "parse failed. " <> show e) (decode str)
    b <- note "aaaaaa!!!" (proc params.body)
    pure $ { b: (makeBody b.trkprm b.sstkn b.csn), c: (makeCookie params.cookie.vil params.cookie.ysc), origin: params.body}

runXHR :: Effect (Fiber Unit)
runXHR = launchAff $ do
  p <- getParameter
  case p of
    Right p' -> do
      -- pure $ makeIframe p'.origin
      postTo p'.c p'.b
    Left e -> liftEffect $ logShow e
