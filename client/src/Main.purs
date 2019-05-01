module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Kerfume.Http (postTo)

main :: Effect Unit
main = do
  a <- postTo
  log "Hello sailor!"
