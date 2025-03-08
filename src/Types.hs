{-# LANGUAGE StrictData #-}

module Types (
  Direction (..),
) where

-- Basic direction type
data Direction = North | South | East | West
  deriving (Show, Eq, Ord)
