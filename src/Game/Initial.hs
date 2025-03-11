module Game.Initial where

import Data.Map (Map)
import Data.Map qualified as Map
import Game.State
import Items
import Rooms

-- Initial room items configuration
initialRoomItems :: Map Room [Item]
initialRoomItems =
  Map.fromList
    [ (ForestClearing, [RustySword, SilverSword])
    , (AbandonedCabin, [BrassLantern])
    , (RiverBank, [WaterFlask, EmptyFlask])
    ]

-- Initial game state
initialState :: GameState
initialState =
  GameState
    { currentRoom = ForestClearing
    , inventory = []
    , roomItems = initialRoomItems
    }
