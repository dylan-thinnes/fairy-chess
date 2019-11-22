module ChessTypes where

import Prelude
import Data.Map (Map)
import Data.Set (Set)
import Data.Array ((..))
import Data.Array as Array
import Data.Maybe
import Data.Tuple
import Data.Vec
import Data.Typelevel.Num as Num
import Data.Typelevel.Undefined (undefined)
import Partial.Unsafe (unsafePartial)

data BasicGameAction locType pieceType
    = Move pieceType (Destination locType) (Destination locType)
    | Remove (Destination locType) pieceType
    | Insert (Destination locType) pieceType

data GameAction locType pieceType
    = Promote (Destination locType) pieceType
    | Threaten (Destination locType) pieceType
    | Clear (Destination locType)
    | Basic (BasicGameAction locType pieceType)

data GameState locType pieceType = GameState
    { board :: Map locType pieceType
    , moveLog :: { log :: Array (GameAction locType pieceType)
                 , perPieceLog :: Map pieceType (Array (GameAction locType pieceType))
                 }
    , threatened :: Set locType
    }

data Destination locType = Absolute locType | Relative locType
data MoveSpec locType pieceType
    = Jump (Array (Precondition locType pieceType)) 
           (Destination locType)
           (Maybe Modifier)
           (Array (GameAction locType pieceType))
    | Repeat (MoveSpec locType pieceType)
    | UpTo Int (MoveSpec locType pieceType)
    | Or (MoveSpec locType pieceType) (MoveSpec locType pieceType)
    | Seq (MoveSpec locType pieceType) (MoveSpec locType pieceType)

data Precondition locType pieceType
    = NeverMoved pieceType
    | Unthreatened (Destination locType)
    | Unoccupied (Destination locType)
    | OccupiedByOpponent (Destination locType) (Maybe pieceType)
    | OccupiedBySelf (Destination locType) (Maybe pieceType)

data VanillaPiece = Pawn1 | Pawn2   | Pawn3   | Pawn4 | Pawn5 | Pawn6   | Pawn7   | Pawn8
                  | Rook1 | Knight1 | Bishop1 | King  | Queen | Bishop2 | Knight2 | Rook2
data VanillaTeam = White | Black
derive instance eqPiece :: Eq VanillaPiece
derive instance eqTeam :: Eq VanillaTeam

data QuickMoveSpec locType pieceType state 
    = Finally (Array (Precondition locType pieceType)) 
              (Array (GameAction locType pieceType))
    | Next state (state -> Maybe (Tuple state (QuickMoveSpec locType pieceType state)))
    | Choose (Array (QuickMoveSpec locType pieceType state))

data MoveTrace locType pieceType state
    = MoveTrace (Array (QuickMoveSpec locType pieceType state))

onlyCapture :: forall pieceType locType state.
               pieceType -> Destination locType -> Destination locType 
            -> QuickMoveSpec locType pieceType state
onlyCapture piece from to = Finally [OccupiedByOpponent to Nothing] [Basic $ Move piece from to, Clear to]

onlyMove :: forall pieceType locType state.
            pieceType -> Destination locType -> Destination locType 
         -> QuickMoveSpec locType pieceType state
onlyMove piece from to = Finally [Unoccupied to] [Basic $ Move piece from to]

moveOrCapture :: forall pieceType locType state.
                 pieceType -> Destination locType -> Destination locType 
              -> QuickMoveSpec locType pieceType state
moveOrCapture piece from to = Choose [onlyCapture piece from to, onlyMove piece from to]

moveUnthreatened :: forall pieceType locType state.
                    pieceType -> Destination locType -> Destination locType 
                 -> QuickMoveSpec locType pieceType state
moveUnthreatened piece from to
  = Choose 
      [ Finally [Unthreatened to, Unoccupied to] [Basic $ Move piece from to]
      , Finally [Unthreatened to, OccupiedByOpponent to Nothing] [Basic $ Move piece from to, Clear to]
      ]

repeatN :: forall pieceType locType state.
           Int
        -> QuickMoveSpec locType pieceType Int
        -> QuickMoveSpec locType pieceType Int
repeatN n moveSpec = Next n f
    where
    f x | x == 0 = Nothing
        | otherwise = Just $ Tuple (x - 1) moveSpec

repeatInfinite :: forall pieceType locType state.
                  QuickMoveSpec locType pieceType state
               -> QuickMoveSpec locType pieceType state
repeatInfinite moveSpec = Next undefined f
    where
    f u = Just $ Tuple u moveSpec

addPreconditions :: forall pieceType locType state.
                   Array (Precondition locType pieceType)
                -> QuickMoveSpec locType pieceType state
                -> QuickMoveSpec locType pieceType state
addPreconditions newPreconds (Finally preconds postconds) 
  = Finally (preconds <> newPreconds) postconds
addPreconditions newPreconds quickMoveSpec = quickMoveSpec

class Piece pieceType locType team where
    moveSpec :: team -> pieceType -> Set locType

type XY = Vec Num.D2 Int
data Modifier
    = NoMirror     
    | MirrorOverX  
    | MirrorOverY  
    | MirrorOver45 
    | Mirror4Ways  
    | Mirror8Ways  

-- Project a position up into higher dimensions
projectPosition :: forall s s' a. (Num.Succ s s') => Vec s Int -> Array (Vec s' Int)
projectPosition vec = map f $ 0 .. length vec
    where
    arr = toArray vec
    f i = unsafePartial $ fromJust $ fromArray =<< Array.insertAt i 0 arr
