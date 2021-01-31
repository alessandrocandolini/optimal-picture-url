module Helpers where

import Data.Map (Map)
import qualified Data.Map as Map

values :: Map k a -> [a]
values = fmap snd . Map.toList
