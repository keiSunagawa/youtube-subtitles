module Kerfume.Regex(
  parsePostFormParams
  ) where

import Data.Array
import Data.Maybe
import Data.String
import Data.String.Pattern
import Prelude

import Data.Array.NonEmpty (last)
import Data.Either (Either, hush)
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)

parsePostFormParams :: String -> Maybe { sstkn :: String, trkprm :: String, csn :: String }
parsePostFormParams s = do
           tr <- (hush $ getTrkprm s) >>= identity
           ss <- (hush $ getSstkn s) >>= identity
           csn <- (hush $ getCsn s) >>= identity
           pure { sstkn: ss, trkprm: tr, csn: csn }

trkprmR = "SUBTITLES\".,\"serviceEndpoint\":.\"clickTrackingParams\":\"([a-zA-Z0-9=_-]*)\""
getTrkprm s = matchLast s trkprmR

sstknR = "XSRF_TOKEN\":\"([a-zA-Z0-9=_-]*)\""
getSstkn s = matchLast s sstknR

csnR = "csn: \"([a-zA-Z0-9=_-]*)\""
getCsn s = matchLast s csnR

matchLast :: String -> String -> Either String (Maybe String)
matchLast s r = do
  re <- R.regex r noFlags
  pure $ do
    xs <- R.match re s
    last xs
