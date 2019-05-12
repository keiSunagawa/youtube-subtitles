module Kerfume.FetchParameters where

import Control.Monad.Except
import Data.Either
import Foreign
import Foreign.Index
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Traversable (sequence)
import Effect.Class.Console (logShow)
import Foreign.Generic (defaultOptions, genericDecodeJSON)

newtype Parameters = Parameters { cookie :: { vil :: String, ysc :: String }
                                , body :: String
                                }

derive instance gnrcParams :: Generic Parameters _
instance showParams :: Show Parameters where show = genericShow

opts = defaultOptions { unwrapSingleConstructors = true }
decode :: String -> Either MultipleErrors Parameters
decode s = runExcept (genericDecodeJSON opts s :: _ Parameters)

decodeP :: Foreign -> F (Array String)
decodeP v = do
  cgs <- v ! "data" ! "actions" ! 0 ! "openTranscriptAction"
        ! "transcriptRenderer" ! "transcriptRenderer" ! "body" ! "transcriptBodyRenderer" ! "cueGroups" >>= readArray
  xs <- sequence $ decodeCueGroup <$> cgs
  pure xs

decodeCueGroup :: Foreign -> F String
decodeCueGroup fg = fg ! "transcriptCueGroupRenderer" ! "cues" ! 0 ! "transcriptCueRenderer" ! "cue" ! "simpleText" >>= readString

foreign import dbg :: forall a. a -> String
