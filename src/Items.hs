{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Items (
  -- Core types
  Item (..),
  ItemType (..),
  Adjective (..),
  -- Core type-to-string conversions
  itemTypeName,
  adjectiveName,
  itemName,
  -- Item property accessors
  itemType,
  itemDescription,
  itemAdjectives,
  -- Item matching and collections
  matchesItem,
  matchItem,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

data ItemType
  = Sword
  | Lantern
  | Flask
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

itemTypeName :: ItemType -> Text
itemTypeName Sword = "sword"
itemTypeName Lantern = "lantern"
itemTypeName Flask = "flask"

data Adjective
  = Rusty
  | Silver
  | Brass
  | Metal
  | Water
  | Empty
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

adjectiveName :: Adjective -> Text
adjectiveName Rusty = "rusty"
adjectiveName Silver = "silver"
adjectiveName Brass = "brass"
adjectiveName Metal = "metal"
adjectiveName Water = "water"
adjectiveName Empty = "empty"

data Item
  = RustySword
  | SilverSword
  | BrassLantern
  | WaterFlask
  | EmptyFlask
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ItemDef = ItemDef
  { dItemType :: ItemType
  , dAdjectives :: Set Adjective
  , dDescription :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

itemDef :: Item -> ItemDef
itemDef RustySword = ItemDef Sword [Rusty, Metal] "An old rusty sword. It's not much, but it's better than nothing."
itemDef SilverSword = ItemDef Sword [Silver, Metal] "A finely crafted silver sword. It gleams in the light."
itemDef BrassLantern = ItemDef Lantern [Brass, Metal] "A well-crafted brass lantern. It could help light up dark places."
itemDef WaterFlask = ItemDef Flask [Water] "A flask filled with clear water."
itemDef EmptyFlask = ItemDef Flask [Empty] "An empty flask that could be used to carry liquids."

itemType :: Item -> ItemType
itemType = dItemType . itemDef

itemAdjectives :: Item -> Set Adjective
itemAdjectives = dAdjectives . itemDef

itemDescription :: Item -> Text
itemDescription = dDescription . itemDef

itemName :: Item -> Text
itemName i = T.unwords (map adjectiveName (Set.toList (itemAdjectives i))) <> " " <> itemTypeName (itemType i)

matchesItem :: ItemType -> [Adjective] -> Item -> Bool
matchesItem reqType reqAdjs item =
  dItemType == reqType && all (`Set.member` dAdjectives) reqAdjs
 where
  ItemDef{dItemType, dAdjectives} = itemDef item

matchItem :: [Adjective] -> ItemType -> [Item] -> [Item]
matchItem reqAdjs reqType = filter (matchesItem reqType reqAdjs)