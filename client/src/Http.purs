module Kerfume.Http where

import Prelude
import Kerfume.Relay

import Effect.Aff (launchAff)


cookie = RelayCookies []
headers = RelayHeaders []
body = Form { body: [] }
url = "http://example.com"

postTo = launchAff $ postRelay url cookie headers body
