module Chess where

import Prelude

import Data.Map as M
import Data.Array
import Data.Maybe
import Data.Ord (abs)

import Data.Lens (Lens, Lens', Traversal', set, view, over, preview)
import Data.Lens.Index (ix)

import Control.Monad.State

type Board = Array Row
type Row   = Array Tile
type Tile  = Maybe Piece

data Piece = Piece Team PieceType
data PieceType = Pawn1 | Pawn2   | Pawn3   | Pawn4 | Pawn5 | Pawn6   | Pawn7   | Pawn8
               | Rook1 | Knight1 | Bishop1 | King  | Queen | Bishop2 | Knight2 | Rook2
data Team = White | Black
derive instance eqTeam :: Eq Team
derive instance eqPieceType :: Eq PieceType
derive instance eqPiece :: Eq Piece

isPawn :: PieceType -> Boolean
isPawn Pawn1 = true
isPawn Pawn2 = true
isPawn Pawn3 = true
isPawn Pawn4 = true
isPawn Pawn5 = true
isPawn Pawn6 = true
isPawn Pawn7 = true
isPawn Pawn8 = true
isPawn _     = false

type XY = { x :: Int, y :: Int }

add :: XY -> XY -> XY
add { x: x1, y: y1 } { x: x2, y: y2 } = { x: x1 + x2, y: y1 + y2 }

startingBoard :: Board
startingBoard
  = [ Just <<< Piece Black <$> [Rook1, Knight1, Bishop1, Queen, King , Bishop2, Knight2, Rook2]
    , Just <<< Piece Black <$> [Pawn1, Pawn2  , Pawn3  , Pawn4, Pawn5, Pawn6  , Pawn7  , Pawn8]
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow
    , Just <<< Piece White <$> [Pawn1, Pawn2  , Pawn3  , Pawn4, Pawn5, Pawn6  , Pawn7  , Pawn8]
    , Just <<< Piece White <$> [Rook1, Knight1, Bishop1, Queen, King , Bishop2, Knight2, Rook2]
    ]
    where
    emptyRow = replicate 8 Nothing

-- board lenses
toLens :: XY -> Traversal' Board Tile
toLens { x, y } = ix y <<< ix x

getCoord  coord = preview (toLens coord)
setCoord  coord = set     (toLens coord)
editCoord coord = over    (toLens coord)

-- piece movement abilities
areDiagonal :: XY -> XY -> Boolean
areDiagonal { x: x1, y: y1 } { x: x2, y: y2 }
    = abs (x1 - x2) == abs (y1 - y2)

areOrthogonal :: XY -> XY -> Boolean
areOrthogonal { x: x1, y: y1 } { x: x2, y: y2 }
  = (x1 - x2) == 0 || (y1 - y2) == 0

data MoveSpec = Jump (Array Precondition) XY (Array Postcondition)
              | UpTo Int MoveSpec
              | Repeat MoveSpec
              | Or (Array MoveSpec)

data BoardXY = Relative XY | Absolute XY
data Precondition = NeverMoved Piece | Unthreatened XY | Unoccupied XY 
                  | Occupied XY (Maybe Team) (Maybe PieceType)
data Postcondition = Move XY XY | Kill XY

moveSpec :: Piece -> MoveSpec
moveSpec (Piece team pieceType) = spec pieceType
    where
    direction = if team == White then (-1) else 1
    spec pieceType = case pieceType of
             King    -> Or [ Jump [Unthreatened { x: 1,  y: 1  }] { x: 1,  y: 1  } []
                           , Jump [Unthreatened { x: -1, y: 1  }] { x: -1, y: 1  } []
                           , Jump [Unthreatened { x: 1,  y: -1 }] { x: 1,  y: -1 } []
                           , Jump [Unthreatened { x: -1, y: -1 }] { x: -1, y: -1 } []
                           , Jump [Unthreatened { x: 0,  y: 1  }] { x: 0,  y: 1  } []
                           , Jump [Unthreatened { x: 1,  y: 0  }] { x: 1,  y: 0  } []
                           , Jump [Unthreatened { x: 0,  y: -1 }] { x: 0,  y: -1 } []
                           , Jump [Unthreatened { x: -1, y: 0  }] { x: -1, y: 0  } []
                           ]
             Queen   -> Or [spec Rook1, spec Bishop1]
             Rook1   -> Or [ Repeat $ Jump [] { x: 0,  y: 1  } []
                           , Repeat $ Jump [] { x: 0,  y: -1 } []
                           , Repeat $ Jump [] { x: 1,  y: 0  } []
                           , Repeat $ Jump [] { x: -1, y: 0  } []
                           ]
             Rook2   -> spec Rook1
             Bishop1 -> Or [ Repeat $ Jump [] { x: 1,  y: 1  } []
                           , Repeat $ Jump [] { x: -1, y: 1  } []
                           , Repeat $ Jump [] { x: 1,  y: -1 } []
                           , Repeat $ Jump [] { x: -1, y: -1 } []
                           ]
             Bishop2 -> spec Bishop1
             Knight1 -> Or [ Jump [] { x: 1,  y: 2  } []
                           , Jump [] { x: 2,  y: 1  } []
                           , Jump [] { x: 1,  y: -2 } []
                           , Jump [] { x: 2,  y: -1 } []
                           , Jump [] { x: -1, y: 2  } []
                           , Jump [] { x: -2, y: 1  } []
                           , Jump [] { x: -1, y: -2 } []
                           , Jump [] { x: -2, y: -1 } []
                           ]
             Knight2 -> spec Knight1
             pawn    -> Or [ Jump [ Unoccupied { x: 0, y: 2 * direction }
                                  , NeverMoved (Piece team pawn)
                                  ]
                                  { x: 0, y: 2 * direction } []
                           , Jump [Unoccupied { x: 0, y: 1 * direction }] 
                                  { x: 0, y: 1 * direction } []
                           , Jump [] { x: 1,    y: 1 } []
                           , Jump [] { x: (-1), y: 1 } []
                           ]

validMoves :: Board -> XY -> M.Map XY (Array Postcondition)
validMoves board position 
  = case join $ getCoord position board of
        Nothing -> M.empty
        Just piece
            -> execState (foldOver position $ moveSpec piece) M.empty
    where
    foldOver position = case _ of
        Jump preconditions relative postconditions -> do
            let newPosition = add position relative
            modify_ (M.insert newPosition postconditions)
            pure newPosition
        UpTo 1 moveSpec -> foldOver position moveSpec
        UpTo n moveSpec -> do
            -- let newPosition = add position relative
            pure position
        _ -> pure position

