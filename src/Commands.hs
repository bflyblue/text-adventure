module Commands (
  Command (..),
  CommandResult (..),
) where

import GameState (GameState)
import Types (Direction)

-- Command types
data Command
  = Look
  | Inventory
  | Take String
  | Drop String
  | Move Direction
  | Help
  | Quit
  deriving (Show)

-- Command result type
data CommandResult = CommandResult
  { newState :: GameState
  , message :: String
  }