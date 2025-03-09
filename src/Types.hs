{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Types (
  Direction (..),
  dirName,
) where

import Data.Text (Text)

-- Basic direction type
data Direction = North | South | East | West
  deriving (Show, Eq, Ord)

dirName :: Direction -> Text
dirName North = "north"
dirName South = "south"
dirName East = "east"
dirName West = "west"
