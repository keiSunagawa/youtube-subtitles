module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Kerfume.GetSubtitle (runAsync)
import Pux (EffModel, noEffects, onlyEffects, start)
import Pux.DOM.Events (DOMEvent, onChange, onClick, onSubmit, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, form, input, span, h3)
import Text.Smolder.HTML.Attributes (name, type', value)
import Text.Smolder.Markup (text, (#!), (!), Markup)
import Data.Array (uncons)
import Data.Foldable (for_)

data Event = Increment
           | Decrement
           | YoutubeUrlChange DOMEvent
           | GetSubtitles
           | SetSubtitles (Array String)

type State = { count :: Int,
               youtubeUrl :: String,
               subtitles :: Array String
               }

-- | Return a new state (and effects) from each event
foldp :: Event -> State -> EffModel State Event
foldp Increment s = { state: s { count = s.count + 1 }, effects: [] }
foldp Decrement s = { state: s { count = s.count - 1 }, effects: [] }
foldp (YoutubeUrlChange ev) s = noEffects $ s { youtubeUrl = targetValue ev }
foldp GetSubtitles s = onlyEffects s [ Just <<< SetSubtitles <$> runAsync s.youtubeUrl ]
foldp (SetSubtitles sub) s = noEffects $ s { subtitles = sub }

-- | Return markup from the state
view :: State -> HTML Event
view state =
  div do
    h3 $ text "Youtube Subtitle Getter"
    -- button #! onClick (const Increment) $ text "Increment"
    -- span $ text (show state.count)
    -- button #! onClick (const Decrement) $ text "Decrement"
    div do
      input ! type' "text" ! value state.youtubeUrl #! onChange YoutubeUrlChange
      button ! type' "submit" #! onClick (const GetSubtitles) $ text "exec"
      for_ state.subtitles (\x -> div $ text x)

-- | Start and render the app
main :: Effect Unit
main = do
  app <- start
    { initialState: { count: 0, youtubeUrl: "", subtitles: [] }
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
-- main :: Effect Unit
-- main = do
--   f <- run
--   pure unit
