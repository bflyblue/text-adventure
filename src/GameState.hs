{-# LANGUAGE StrictData #-}

module GameState (
  GameState (..),
) where

import Data.Map (Map)
import Items (Item)
import Rooms (Room)

-- Game state type
data GameState = GameState
  { currentRoom :: Room
  , inventory :: [Item]
  , roomItems :: Map Room [Item]
  }
  deriving (Show)