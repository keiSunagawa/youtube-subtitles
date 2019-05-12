module Kerfume.Regex(
  parsePostFormParams
  ) where

import Data.Array
import Data.Maybe
import Data.String
import Data.String.Pattern
import Prelude

import Data.Array.NonEmpty (last)
import Data.Either (Either, note)
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)

parsePostFormParams :: String -> Either String { sstkn :: String, trkprm :: String, csn :: String }
parsePostFormParams s = do
           tr <- getTrkprm s
           ss <- getSstkn s
           csn <- getCsn s
           pure { sstkn: ss, trkprm: tr, csn: csn }

trkprmR = "SUBTITLES\".,\"serviceEndpoint\":.\"clickTrackingParams\":\"([a-zA-Z0-9=_-]*)\""
getTrkprm s = matchLast s trkprmR

sstknR = "XSRF_TOKEN\":\"([a-zA-Z0-9=_-]*)\""
getSstkn s = matchLast s sstknR

csnR = "csn: \"([a-zA-Z0-9=_-]*)\""
getCsn s = matchLast s csnR

matchLast :: String -> String -> Either String String
matchLast s r = do
  re <- R.regex r noFlags
  note (matchError r) $ do
    xs <- R.match re s
    last xs

matchError r = "unmatch regex" <> r
