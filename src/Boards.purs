module Boards where

import Locations

import Data.Maybe

class IsBoard slot location | slot -> location where
    unrel :: slot -> location -> location -> Maybe location
    valid :: slot -> location -> Boolean

resolve :: forall slot location. (IsBoard slot location) 
        => slot -> location -> AbsOrRel location -> Maybe location
resolve slot origin (Relative location) = unrel slot origin location
resolve slot origin (Absolute location) = if valid slot location then Just location else Nothing
