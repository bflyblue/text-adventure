{-# LANGUAGE StrictData #-}

module Commands (
  Command (..),
  CommandResult (..),
) where

import Data.Text (Text)
import GameState (GameState)
import Items (Adjective, ItemType)
import Types (Direction)

-- Command types
data Command
  = Look
  | Examine [Adjective] ItemType
  | Inventory
  | Take [Adjective] ItemType
  | Drop [Adjective] ItemType
  | Move Direction
  | Help
  | Quit
  deriving (Show)

-- Command result type
data CommandResult = CommandResult
  { newState :: GameState
  , message :: Text
  }