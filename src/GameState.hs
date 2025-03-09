{-# LANGUAGE StrictData #-}

module GameState (
  GameState (..),
  getRoomItems,
  takeItemFromRoom,
  dropItemToRoom,
  addItemToRoom,
  removeItemFromRoom,
  addItemToInventory,
  removeItemFromInventory,
  initialRoomItems,
  initialState,
) where

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Items (Item (..))
import Rooms (Room (..))

-- Game state type
data GameState = GameState
  { currentRoom :: Room
  , inventory :: [Item]
  , roomItems :: Map Room [Item]
  }
  deriving (Show)

-- Helper functions for room state
getRoomItems :: Room -> GameState -> [Item]
getRoomItems room state = Map.findWithDefault [] room (roomItems state)

takeItemFromRoom :: Room -> Item -> GameState -> GameState
takeItemFromRoom room item =
  addItemToInventory item . removeItemFromRoom room item

dropItemToRoom :: Room -> Item -> GameState -> GameState
dropItemToRoom room item =
  removeItemFromInventory item . addItemToRoom room item

addItemToRoom :: Room -> Item -> GameState -> GameState
addItemToRoom room item state =
  state{roomItems = Map.adjust (item :) room (roomItems state)}

removeItemFromRoom :: Room -> Item -> GameState -> GameState
removeItemFromRoom room item state =
  state{roomItems = Map.adjust (filter (/= item)) room (roomItems state)}

addItemToInventory :: Item -> GameState -> GameState
addItemToInventory item state =
  state{inventory = item : inventory state}

removeItemFromInventory :: Item -> GameState -> GameState
removeItemFromInventory item state =
  state{inventory = filter (/= item) (inventory state)}

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
