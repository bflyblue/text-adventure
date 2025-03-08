{-# LANGUAGE StrictData #-}

module Rooms (
  Room (..),
  roomDescriptions,
  roomNames,
  roomExits,
  getRoomDescription,
  getRoomName,
  getRoomExits,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Types (Direction (..))

-- Room type
data Room
  = ForestClearing
  | AbandonedCabin
  | RiverBank
  deriving (Show, Eq, Ord)

-- Room descriptions
roomDescriptions :: Map Room String
roomDescriptions =
  Map.fromList
    [ (ForestClearing, "You are in a peaceful forest clearing. Sunlight filters through the leaves above.")
    , (AbandonedCabin, "An old wooden cabin stands here, its door hanging loose on rusty hinges.")
    , (RiverBank, "A gentle river flows here. The water looks cool and refreshing.")
    ]

-- Room names
roomNames :: Map Room String
roomNames =
  Map.fromList
    [ (ForestClearing, "Forest Clearing")
    , (AbandonedCabin, "Abandoned Cabin")
    , (RiverBank, "River Bank")
    ]

-- Room exits
roomExits :: Map Room (Map Direction Room)
roomExits =
  Map.fromList
    [ (ForestClearing, Map.fromList [(North, AbandonedCabin), (East, RiverBank)])
    , (AbandonedCabin, Map.fromList [(South, ForestClearing)])
    , (RiverBank, Map.fromList [(West, ForestClearing)])
    ]

-- Helper functions
getRoomDescription :: Room -> String
getRoomDescription room = Map.findWithDefault "You are in an undefined space." room roomDescriptions

getRoomName :: Room -> String
getRoomName room = Map.findWithDefault "Unnamed Room" room roomNames

getRoomExits :: Room -> Map Direction Room
getRoomExits room = Map.findWithDefault Map.empty room roomExits