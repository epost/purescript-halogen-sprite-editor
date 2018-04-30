module SpriteEditor
  ( component
  , State
  , Query
  ) where

import Prelude hiding (div)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Sprite as Sprite
import Data.Sprite (Sprite)
import Halogen as H
import Halogen (ComponentDSL, ComponentHTML)
import Halogen.HTML (HTML, div, div_, h1, h1_, p, p_, text, button)
import Halogen.HTML.Events (onClick, input_)
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Core (ClassName(..))

data Query a = ToggleState Int Int a

type State = { sprite :: Sprite Boolean }

component :: forall m. State -> H.Component HTML Query _ Void m
component initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> ComponentHTML Query
    render state =
      div []
          [ h1 [] [ text "Sprite Editor by Erik / Shinsetsu" ]
          , renderPixelRows state.sprite
          ]

    eval :: Query ~> ComponentDSL State Query Void m
    eval = case _ of
      ToggleState x y next -> do
        H.modify (\state -> { sprite: Sprite.modifyAt state.sprite not x y })
        pure next

pixelStateClass :: Boolean -> ClassName
pixelStateClass p = ClassName $ if p then "foreground-pixel" else "background-pixel"

renderPixel :: Int -> Int -> Boolean -> HTML Void (Query _)
renderPixel y x p =
  div [ classes [ ClassName "pixel"
                , ClassName ("pixel-at-col-" <> show x)
                , ClassName ("pixel-at-row-" <> show y)
                , pixelStateClass p
                ]
      , onClick (input_ $ ToggleState x y)
      ]
      []

renderPixelRow :: Int -> Array Boolean -> HTML Void (Query _)
renderPixelRow y pixels =
  div [ classes [ ClassName "pixel-row" ] ]
      (mapWithIndex (renderPixel y) pixels)

renderPixelRows :: Sprite Boolean -> HTML Void (Query _)
renderPixelRows spr =
  div [ classes [ ClassName "pixel-rows" ] ]
      (renderPixelRow `mapWithIndex` spr)
