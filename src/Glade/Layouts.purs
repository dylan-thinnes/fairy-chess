module Glade.Layouts where

import Glade.Boards
import Glade.Pieces

import Data.Maybe
import Data.Tuple (Tuple)

class ( IsPiece slot name team location, IsBoard slot location )
     <= IsLayout slot name team location
      | slot -> name team location where
    isBackRank :: slot -> team -> location -> Boolean
    initialState :: slot -> Array (Tuple (Piece name team) location)
