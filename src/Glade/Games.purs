module Glade.Games where

import Glade.Locations
import Glade.Layouts
import Glade.Pieces
import Glade.Boards

class ( IsLayout slot name team location )
     <= IsGame slot name team location state
      | slot -> name team location where
    performAction :: slot -> GameAction location (Piece name team) -> state -> state
