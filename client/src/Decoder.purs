module Kerfume.Decoder where

import Control.Monad.Except
import Data.Either
import Foreign
import Foreign.Index
import Prelude
import Simple.JSON

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Traversable (sequence)
import Effect.Class.Console (logShow)
import Foreign.Generic (defaultOptions, genericDecodeJSON)

newtype Parameters = Parameters { cookie :: { vil :: String, ysc :: String }
                                , body :: String
                                }

decodeParams :: String -> Either MultipleErrors Parameters
decodeParams s = runExcept (genericDecodeJSON opts s :: _ Parameters)

decodeSubtitle :: String -> Either MultipleErrors (Array String)
decodeSubtitle s = do
  fr <- runExcept $ parseJSON s
  runExcept $ decodeSubtitle' fr

decodeSubtitle' :: Foreign -> F (Array String)
decodeSubtitle' v = do
  cgs <- v ! "data" ! "actions" ! 0 ! "openTranscriptAction"
        ! "transcriptRenderer" ! "transcriptRenderer" ! "body" ! "transcriptBodyRenderer" ! "cueGroups" >>= readArray
  xs <- sequence $ decodeCueGroup <$> cgs
  pure xs

decodeCueGroup :: Foreign -> F String
decodeCueGroup fg = fg ! "transcriptCueGroupRenderer" ! "cues" ! 0 ! "transcriptCueRenderer" ! "cue" ! "simpleText" >>= readString

opts = defaultOptions { unwrapSingleConstructors = true }
derive instance gnrcParams :: Generic Parameters _
instance showParams :: Show Parameters where show = genericShow

foreign import dbg :: forall a. a -> String
