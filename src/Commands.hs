{-# LANGUAGE StrictData #-}

module Commands (
  Command (..),
  CommandResult (..),
) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Items (ItemSelection)
import Types (Direction)

-- Command types
data Command
  = Look
  | Examine ItemSelection
  | Inventory
  | Take ItemSelection
  | Drop ItemSelection
  | Move Direction
  | Help
  | Quit
  deriving (Show, Eq, Ord, Generic)

newtype CommandResult = CommandResult
  { message :: Text
  }
  deriving (Show, Eq, Ord, Generic)