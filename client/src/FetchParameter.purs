module Kerfume.FetchParameters where

import Control.Monad.Except
import Data.Either
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign (MultipleErrors)
import Foreign.Generic (defaultOptions, genericDecodeJSON)

newtype Parameters = Parameters { cookie :: { vil :: String, ysc :: String }
                                , body :: String
                                }

derive instance gnrcParams :: Generic Parameters _
instance showParams :: Show Parameters where show = genericShow

opts = defaultOptions { unwrapSingleConstructors = true }
decode :: String -> Either MultipleErrors Parameters
decode s = runExcept (genericDecodeJSON opts s :: _ Parameters)
