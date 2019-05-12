module Main where

import Prelude

import Effect (Effect)
import Kerfume.GetSubtitle (run)

main :: Effect Unit
main = do
  f <- run
  pure unit
