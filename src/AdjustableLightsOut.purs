module AdjustableLightsOut (component) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Data.Symbol
import Data.Maybe (Maybe(..))

import LightsOut as LightsOut

type GameSlot = (a :: forall q. H.Slot q Void Unit)
gslot = SProxy :: SProxy "a"

data Action = ChangeX Int | ChangeY Int
type State = { x :: Int, y :: Int }

component :: forall q o m. H.Component HH.HTML q Unit o m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
    where

    initialState :: Unit -> State
    initialState = const { x: 3, y: 3 }

    render :: State -> H.ComponentHTML Action GameSlot m
    render state = HH.div [] 
        [ HH.slot gslot unit LightsOut.component state (const Nothing)
        , HH.div []
            [ HH.text $ "X: " <> show state.x
            , HH.button
                [ HP.title "X+"
                , HE.onClick $ \_ -> Just $ ChangeX 1
                ]
                [ HH.text "X+" ]
            , HH.button
                [ HP.title "X-"
                , HE.onClick $ \_ -> Just $ ChangeX (-1)
                ]
                [ HH.text "X-" ]
            ]
        , HH.div []
            [ HH.text $ "Y: " <> show state.y
            , HH.button
                [ HP.title "Y+"
                , HE.onClick $ \_ -> Just $ ChangeY 1
                ]
                [ HH.text "Y+" ]
            , HH.button
                [ HP.title "Y-"
                , HE.onClick $ \_ -> Just $ ChangeY (-1)
                ]
                [ HH.text "Y-" ]
            ]
        ]

    handleAction :: Action -> H.HalogenM State Action GameSlot o m Unit
    handleAction 
      = case _ of
          ChangeX dx -> H.modify_ $ \state -> state { x = state.x + dx }
          ChangeY dy -> H.modify_ $ \state -> state { y = state.y + dy }
