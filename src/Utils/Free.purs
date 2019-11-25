module Utils.Free where

-- A simple implementation of the free monad.
-- Sufficient for our needs in this library.

import Prelude

import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

import Control.Alt (class Alt)
import Control.Plus (class Plus, empty)
import Control.Alternative (class Alternative)
import Control.MonadZero (class MonadZero)

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

