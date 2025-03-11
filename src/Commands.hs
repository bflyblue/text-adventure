{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Commands (
  Command (..),
  CommandResult (..),
) where

import Data.Text (Text)
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
  { message :: Text
  }