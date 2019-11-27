module Glade.Pieces where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Array
import Data.Map (Map)
import Data.Set (Set)
import Data.Unit
import Data.Ord
import Data.Newtype

import Utils.Free
import Glade.Locations

import Control.Alt (class Alt)
import Control.Plus (class Plus, empty)
import Control.Alternative (class Alternative)
import Control.MonadZero (class MonadZero)
import Type.Proxy

data MovementEvent location piece = MovementEvent location piece Int
instance movementEventShow :: (Show location, Show piece)
                           => Show (MovementEvent location piece) where
    show (MovementEvent l p i) = "MovementEvent " <> show l <> " " <> show p <> " " <> show i

data PreconditionNext location piece next
    = NeverMoved piece (Boolean -> next)
    | AtPiece piece (location -> next)
    | UnthreatenedLocation (AbsOrRel location) (Boolean -> next)
    | UnthreatenedPiece piece (Boolean -> next)
    | OccupiedBy (AbsOrRel location) (Maybe piece -> next)
    | MovementHistoryComplete (Array (MovementEvent location piece) -> next)
    | MovementHistoryPiece piece (Array (MovementEvent location piece) -> next)
    | PositionsEqual (AbsOrRel location) (AbsOrRel location) (Boolean -> next)
    | Failure

derive instance preconditionNextFunctor :: Functor (PreconditionNext location piece)
instance preconditionNextShow :: (Show location, Show piece)
                          => Show (PreconditionNext location piece next) where
    show (NeverMoved x _) = "NeverMoved " <> show x <> " (and a function)"
    show (AtPiece x _) = "AtPiece " <> show x <> " (and a function)"
    show (UnthreatenedLocation x _) = "UnthreatenedLocation " <> show x <> " (and a function)"
    show (UnthreatenedPiece x _) = "UnthreatenedPiece " <> show x <> " (and a function)"
    show (OccupiedBy x _) = "OccupiedBy " <> show x <> " (and a function)"
    show (MovementHistoryComplete _) = "MovementHistoryComplete" <> " (and a function)"
    show (MovementHistoryPiece x _) = "MovementHistoryPiece " <> show x <> " (and a function)"
    show (PositionsEqual x y _) = "PositionsEqual " <> show x <> " " <> show y <> " (and a function)"
    show Failure = "Failure"

-- Precondition monad, made from Free monad from PreconditionNext
-- Use a newtype so that Precondition monad can have MonadZero
newtype Precondition location piece a = Precondition (Free (PreconditionNext location piece) a)
derive instance preconditionNewtype :: Newtype (Precondition location piece a) _

-- Synonym for Precondition with Boolean return value
type Pre location piece = Precondition location piece Boolean

-- Instances to turn "Free PreconditionNext" to "Precondition" with MonadZero
instance preconditionFunctor :: Functor (Precondition location piece) where
    map f = wrap <<< map f <<< unwrap
instance preconditionApply :: Apply (Precondition location piece) where
    apply (Precondition (Free Failure)) _ = wrap $ Free Failure
    apply _ (Precondition (Free Failure)) = wrap $ Free Failure
    apply x y                             = wrap $ apply (unwrap x) (unwrap y)
instance preconditionBind :: Bind (Precondition location piece) where
    bind (Precondition (Free Failure)) _ = wrap $ Free Failure
    bind x f                             = wrap $ bind (unwrap x) (unwrap <<< f)
instance preconditionApplicative :: Applicative (Precondition location piece) where
    pure = Precondition <<< Pure
instance preconditionMonad :: Monad (Precondition location piece)

instance preconditionAlt :: Alt (Precondition location piece) where
    alt = (*>)
instance preconditionPlus :: Plus (Precondition location piece) where
    empty = failure
instance preconditionAlternative :: Alternative (Precondition location piece)
instance preconditionMonadZero :: MonadZero (Precondition location piece)

-- PreconditionNext data constructors turned to Preconditions
neverMoved              = wrap <<< toFreeBind1 NeverMoved
atPiece                 = wrap <<< toFreeBind1 AtPiece
unthreatenedLocation    = wrap <<< toFreeBind1 UnthreatenedLocation
unthreatenedPiece       = wrap <<< toFreeBind1 UnthreatenedPiece
occupiedBy              = wrap <<< toFreeBind1 OccupiedBy
movementHistoryComplete = wrap   $ toFreeBind0 MovementHistoryComplete
movementHistoryPiece    = wrap <<< toFreeBind1 MovementHistoryPiece
positionsEqual x        = wrap <<< toFreeBind1 (PositionsEqual x)
failure                 = wrap   $ liftF Failure

-- Project Maybe a to m a, where m is MonadZero
-- Always turn Just a to pure a, Nothing to empty
try :: forall m a. (MonadZero m) => Maybe a -> m a
try Nothing  = empty
try (Just x) = pure x

-- Simple data type expressing all possible fundamental actions in a game
data GameAction location piece
    = Move piece (AbsOrRel location)
    | Promote piece (Array piece)
    | Kill piece
    | Clear (AbsOrRel location)

data QuickMoveSpec location piece
      -- Finish by executing these actions, and then ensure postconds
    = Finally (Array (GameAction location piece))
              (Pre location piece)
      -- Require the following preconditions before trying the following move spec
    | Require (Pre location piece)
              (QuickMoveSpec location piece)
    | Choice (Array (QuickMoveSpec location piece))
      -- If the first succeeds, the second *must* be done
    | Sequence (QuickMoveSpec location piece) (QuickMoveSpec location piece)
      -- If the given move spec fails, succeed with a noop
    | Optionally (QuickMoveSpec location piece)
      -- Repeat the action until it fails
    | Repeat (QuickMoveSpec location piece)

while
    :: forall location piece.
       Pre location piece -> QuickMoveSpec location piece -> QuickMoveSpec location piece
while cond action = Repeat (Require cond action)

locEmpty
    :: forall location piece. (Eq piece) =>
       AbsOrRel location -> Pre location piece
locEmpty loc = (_ == Nothing) <$> occupiedBy loc

repeatMove
    :: forall location piece. (Eq piece) =>
       piece -> AbsOrRel location -> QuickMoveSpec location piece
repeatMove self location = Repeat $ passiveMoveOnly self location

passiveMoveOnly
    :: forall location piece. (Eq piece) =>
       piece -> AbsOrRel location -> QuickMoveSpec location piece
passiveMoveOnly self location
    = Require (locEmpty location) (Finally [Move self location] $ pure true)

attackMoveOnly
    :: forall location name team. (Eq team) =>
       Piece name team -> AbsOrRel location -> QuickMoveSpec location (Piece name team)
attackMoveOnly self location
    = Require (occupiedBy location >>= try >>= \occupant -> pure $ team occupant /= team self)
              (Finally [Clear location, Move self location] $ pure true)

passiveOrAttackMove
    :: forall location name team. (Eq team) =>
       Piece name team -> AbsOrRel location -> QuickMoveSpec location (Piece name team)
passiveOrAttackMove self location
    = Finally [Clear location, Move self location] $ pure true

repeatMoveUntilMaybeAttack
    :: forall location name team. (Eq team) => (Eq name) =>
       Piece name team -> AbsOrRel location -> QuickMoveSpec location (Piece name team)
repeatMoveUntilMaybeAttack self location
    = Sequence (repeatMove self location) $ Optionally (attackMoveOnly self location)

data GameState location piece board
    = GameState { history :: (Array (GameStep location piece))
                , state :: Map piece location
                }
data GameStep location piece
    = GameStep { actions :: Array (GameAction location piece)
               , inCheck :: Boolean
               }

data MoveTrace location piece
    = MoveTrace (Array (QuickMoveSpec location piece))

data Piece name team = Piece name team Int
name :: forall name team. Piece name team -> name
name (Piece n t i) = n
team :: forall name team. Piece name team -> team
team (Piece n t i) = t
id :: forall name team. Piece name team -> Int
id (Piece n t i) = i

instance eqPiece :: (Eq name, Eq team) => Eq (Piece name team) where
    eq (Piece n1 t1 i) (Piece n2 t2 j) = n1 == n2 && t1 == t2 && i == j

sameType :: forall name team.
            (Eq name) => (Eq team) =>
            Piece name team -> Piece name team -> Boolean
sameType (Piece n1 t1 _) (Piece n2 t2 _) = n1 == n2 && t1 == t2

data State piece board = State

class IsPiece slot name team location | slot -> name team location where
    toMoveSpec :: (Eq name) => (Eq team) =>
                  slot -> (Piece name team) -> QuickMoveSpec location (Piece name team)
    initialState :: slot -> Array (Tuple (Piece name team) location)
