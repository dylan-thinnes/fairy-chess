module Glade.Locations where

import Prelude
import Data.Newtype
import Data.Foldable

data AbsOrRel location = Absolute location | Relative location
instance absOrRelShow :: (Show location) => Show (AbsOrRel location) where
    show (Absolute l) = "Abs " <> show l
    show (Relative l) = "Rel " <> show l

data D2 = D2 { x :: Int, y :: Int }
instance showD2 :: Show D2 where
    show (D2 { x, y }) = "D2 " <> show x <> " " <> show y
data D3 = D3 { x :: Int, y :: Int, z :: Int }

mirrorX :: D2 -> D2
mirrorX (D2 { x, y }) = D2 { x: x, y: -y }

mirrorY :: D2 -> D2
mirrorY (D2 { x, y }) = D2 { x: -x, y: y }

mirrorXY :: D2 -> D2
mirrorXY (D2 { x, y }) = D2 { x: y, y: x }

neg :: D2 -> D2
neg (D2 { x, y }) = D2 { x: -x, y: -y }

newtype Mirror = Mirror (D2 -> Array D2)
derive instance mirrorNewtype :: Newtype Mirror _

toMirror :: (D2 -> D2) -> Mirror
toMirror f = Mirror $ \x -> [f x, x]

instance semigroupMirror :: Semigroup Mirror where
    append f g = wrap $ \x -> unwrap f x >>= unwrap g

instance monoidMirror :: Monoid Mirror where
    mempty = wrap $ \x -> [x]

sequenceMirrors :: Array Mirror -> Mirror
sequenceMirrors = fold

runMirror :: Mirror -> D2 -> Array D2
runMirror = unwrap

octopize :: D2 -> Array D2
octopize c@(D2 { x, y }) = unwrap (foldMap toMirror transforms) c
    where
    transforms | x == 0 && y == 0 = []
               | x == 0           = [mirrorX, mirrorXY]
               | y == 0           = [mirrorY, mirrorXY]
               | x == y           = [mirrorX, mirrorY]
               | otherwise        = [mirrorX, mirrorY, mirrorXY]
