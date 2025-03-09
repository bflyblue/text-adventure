{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Rooms (
  Room (..),
  roomDescription,
  roomName,
  roomExits,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Types (Direction (..))

-- Room type
data Room
  = ForestClearing
  | AbandonedCabin
  | RiverBank
  deriving (Show, Eq, Ord, Enum, Bounded)

roomDescription :: Room -> Text
roomDescription ForestClearing = "You are in a peaceful forest clearing. Sunlight filters through the leaves above."
roomDescription AbandonedCabin = "An old wooden cabin stands here, its door hanging loose on rusty hinges."
roomDescription RiverBank = "A gentle river flows here. The water looks cool and refreshing."

roomName :: Room -> Text
roomName ForestClearing = "Forest Clearing"
roomName AbandonedCabin = "Abandoned Cabin"
roomName RiverBank = "River Bank"

roomExits :: Room -> Map Direction Room
roomExits ForestClearing = Map.fromList [(North, AbandonedCabin), (East, RiverBank)]
roomExits AbandonedCabin = Map.fromList [(South, ForestClearing)]
roomExits RiverBank = Map.fromList [(West, ForestClearing)]
