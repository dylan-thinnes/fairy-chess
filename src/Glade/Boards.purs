module Glade.Boards where

import Glade.Locations

import Prelude
import Data.Maybe
import Data.Array

class IsBoard slot location | slot -> location where
    unrel :: slot -> location -> location -> location
    simplify :: slot -> location -> Maybe location
    locations :: slot -> Maybe (Array location)

resolve :: forall slot location. (IsBoard slot location)
        => slot -> location -> AbsOrRel location -> Maybe location
resolve slot origin (Relative location) = simplify slot $ unrel slot origin location
resolve slot origin (Absolute location) = simplify slot location
