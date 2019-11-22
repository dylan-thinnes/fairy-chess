module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import AdjustableLightsOut as AdjustableLightsOut
import ChessComponent as Chess

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Chess.component unit body
