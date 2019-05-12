module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff, try)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Kerfume.Http (getParameter, postTo, runXHR)

main :: Effect Unit
main = do
  f <- runXHR
  pure unit

-- main :: Effect Unit
-- main = do
--   f <- launchAff $ getParameter >>= (\x -> liftEffect $ logShow x)
--   pure unit
