module Movement where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Array
import Data.Map (Map)
import Data.Set (Set)
import Data.Unit
import Data.Ord
import Control.Alt (class Alt)
import Control.Plus (class Plus, empty)
import Control.Alternative (class Alternative)
import Control.MonadZero (class MonadZero)
import Type.Proxy

import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

newtype Fix f = Fix (f (Fix f))
data Free f r = Free (f (Free f r)) | Pure r

instance freeShow :: (Show a, Show (f (Free f a))) => Show ((Free f) a) where
    show (Free x) = "Free " <> show x
    show (Pure x) = "Pure " <> show x

instance freeFunctor :: (Functor f) => Functor (Free f) where
    map f x = x >>= f >>> Pure

instance freeApplicative :: (Functor f) => Applicative (Free f) where
    pure = Pure

instance freeApply :: (Functor f) => Apply (Free f) where
    apply = ap

instance freeBind :: (Functor f) => Bind (Free f) where
    bind (Free x) f = Free $ map (_ >>= f) x
    bind (Pure x) f = f x

liftF :: forall f r. Functor f => f r -> Free f r
liftF x = Free (map Pure x)

instance freeMonad :: (Functor f) => Monad (Free f)

instance freeMonadTrans :: MonadTrans Free where
    lift = liftF

data AbsOrRel location = Absolute location | Relative location
instance absOrRelShow :: (Show location) => Show (AbsOrRel location) where
    show (Absolute l) = "Abs " <> show l
    show (Relative l) = "Rel " <> show l

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

derive instance preconditionFunctor :: Functor (PreconditionNext location piece)
instance preconditionShow :: (Show location, Show piece) 
                          => Show (PreconditionNext location piece next) where
    show (NeverMoved x _) = "NeverMoved " <> show x
    show (AtPiece x _) = "AtPiece " <> show x
    show (UnthreatenedLocation x _) = "UnthreatenedLocation " <> show x
    show (UnthreatenedPiece x _) = "UnthreatenedPiece " <> show x
    show (OccupiedBy x _) = "OccupiedBy " <> show x
    show (MovementHistoryComplete _) = "MovementHistoryComplete"
    show (MovementHistoryPiece x _) = "MovementHistoryPiece " <> show x
    show (PositionsEqual x y _) = "PositionsEqual " <> show x <> " " <> show y
    show Failure = "Failure"

type Precondition location piece = Free (PreconditionNext location piece) Boolean
instance freePreconditionAlt :: Alt (Free (PreconditionNext location piece)) where
    alt = (*>)
instance freePreconditionPlus :: Plus (Free (PreconditionNext location piece)) where
    empty = failure
instance freePreconditionAlternative :: Alternative (Free (PreconditionNext location piece))
instance freePreconditionMonadZero :: MonadZero (Free (PreconditionNext location piece))

toFreeJoin0 :: forall f. (Functor f) 
           => (Unit -> f Unit) -> Free f Unit
toFreeJoin0 constructor = liftF $ constructor unit

toFreeJoin1 :: forall a f. (Functor f) 
           => (a -> Unit -> f Unit) -> a -> Free f Unit
toFreeJoin1 constructor value = liftF $ constructor value unit

toFreeBind0 :: forall b f. (Functor f) 
            => ((b -> b) -> f b) -> Free f b
toFreeBind0 constructor = liftF $ constructor identity

toFreeBind1 :: forall a1 b f. (Functor f) 
            => (a1 -> (b -> b) -> f b) -> a1 -> Free f b
toFreeBind1 constructor v1 = liftF $ constructor v1 identity

toFreeBind2 :: forall a1 a2 b f. (Functor f) 
            => (a1 -> a2 -> (b -> b) -> f b) -> a1 -> a2 -> Free f b
toFreeBind2 constructor v1 v2 = liftF $ constructor v1 v2 identity

neverMoved = toFreeBind1 NeverMoved
atPiece = toFreeBind1 AtPiece
unthreatenedLocation = toFreeBind1 UnthreatenedLocation
unthreatenedPiece = toFreeBind1 UnthreatenedPiece
occupiedBy = toFreeBind1 OccupiedBy
movementHistoryComplete = toFreeBind0 MovementHistoryComplete
movementHistoryPiece = toFreeBind1 MovementHistoryPiece
positionsEqual = toFreeBind2 PositionsEqual
failure = liftF Failure

try :: forall m a. (MonadZero m) => Maybe a -> m a
try Nothing  = empty
try (Just x) = pure x

data GameAction location piece
    = Move piece (AbsOrRel location)
    | Promote piece (Array piece)
    | Kill piece
    | Clear (AbsOrRel location)

data QuickMoveSpec location piece
    = Finally (Array (GameAction location piece))
              (Precondition location piece)
    | Require (Precondition location piece)
              (QuickMoveSpec location piece)
    | Choice (Array (QuickMoveSpec location piece))
    | Sequence (QuickMoveSpec location piece) (QuickMoveSpec location piece)

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

instance eqPiece :: (Eq name, Eq team) => Eq (Piece name team) where
    eq (Piece n1 t1 i) (Piece n2 t2 j) = n1 == n2 && t1 == t2 && i == j

sameType :: forall name team. 
            (Eq name) => (Eq team) => 
            Piece name team -> Piece name team -> Boolean
sameType (Piece n1 t1 _) (Piece n2 t2 _) = n1 == n2 && t1 == t2

data State piece board = State

class IsPiece slot name team location | name -> name team location where
    toMoveSpec :: (Eq name) => (Eq team) => slot -> (Piece name team) -> QuickMoveSpec location (Piece name team)
    initialState :: slot -> Array (Tuple (Piece name team) location)

data D2 = D2 { x :: Int, y :: Int }
data D3 = D3 { x :: Int, y :: Int, z :: Int }
