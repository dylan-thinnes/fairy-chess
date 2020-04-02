module Glade.Games where

import Prelude

import Glade.Locations
import Glade.Layouts
import Glade.Pieces
import Glade.Boards

import Data.Array
import Data.Map
import Data.Set

data GameState extra name team location piece 
    = GameState 
        { extra :: extra
        , pieces :: Map location piece
        , threats :: Map (Piece name team) (Set location)
        }

class ( IsLayout slot name team location )
     <= IsGame slot name team location extra
      | slot -> name team location extra where
    performAction :: slot -> GameAction location (Piece name team) 
                  -> GameState extra name team location (Piece name team)
                  -> GameState extra name team location (Piece name team)
    evaluatePrecondition :: slot -> Pre location (Piece name team) -> Boolean
    customPostconditions :: slot -> team -> Array (Pre location (Piece name team))

defaultActionPerformer :: forall location name team extra. 
                          GameAction location (Piece name team)
                       -> GameState extra name team location (Piece name team)
                       -> GameState extra name team location (Piece name team)
defaultActionPerformer (Move piece location) = identity
defaultActionPerformer (Promote piece pieces) = identity
defaultActionPerformer (Kill piece) = identity
defaultActionPerformer (Clear location) = identity

