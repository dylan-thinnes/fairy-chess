module Layouts where

import Boards
import Pieces

import Data.Maybe
import Data.Tuple (Tuple)

class ( IsPiece slot name team location, IsBoard slot location )
     <= IsLayout slot name team location
      | slot -> name team location where
    initialState :: slot -> Array (Tuple (Piece name team) location)
    isBackRank :: slot -> team -> location -> Boolean
