module Button (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { enabled :: Boolean, count :: Int }
data Action = Toggle

component :: forall q i o m. H.Component HH.HTML q i o m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }

    where

    initialState :: forall a. a -> State
    initialState = const { enabled: false, count: 0 }

    render :: forall m. State -> H.ComponentHTML Action () m
    render state =
      let
        label = if state.enabled then "On" else "Off"
      in
        HH.div []
          [ HH.button
              [ HP.title label
              , HE.onClick \_ -> Just Toggle
              ]
              [ HH.text label ]
          , HH.text $ show state.count
          ]

    handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
    handleAction = case _ of
      Toggle -> do
        H.modify_ \st -> st { enabled = not st.enabled }
        enabled <- H.gets \st -> st.enabled
        if enabled 
          then H.modify_ \st -> st { count = st.count + 1 }
          else pure unit
