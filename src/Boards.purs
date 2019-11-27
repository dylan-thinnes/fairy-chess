module Boards where

import Data.Maybe

class IsBoard slot location | slot -> location where
    unrel :: slot -> location -> location -> Maybe location
    valid :: slot -> location -> Boolean


