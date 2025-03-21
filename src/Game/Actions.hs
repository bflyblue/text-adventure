module Game.Actions where

import Data.Map.Strict qualified as Map
import Game.State
import Items (Item)
import Rooms (Room)

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

moveToRoom :: Room -> GameState -> GameState
moveToRoom room state =
  state{currentRoom = room}
