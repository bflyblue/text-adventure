module Items (
  Item (..),
  ItemType (..),
  Adjective (..),
  getItemName,
  getItemDescription,
  getItemType,
  getItemAdjectives,
  matchItem,
) where

import Data.Char (toLower)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

-- Base item types
data ItemType
  = Sword
  | Lantern
  | Flask
  deriving (Show, Eq, Ord)

-- Adjectives for items
data Adjective
  = Rusty
  | Silver
  | Brass
  | Metal
  | Water
  | Empty
  deriving (Show, Eq, Ord)

-- Full item type with descriptor
data Item
  = RustySword -- A rusty, metal sword
  | SilverSword -- A silver, metal sword
  | BrassLantern -- A brass, metal lantern
  | WaterFlask -- A water flask
  | EmptyFlask -- An empty flask
  deriving (Show, Eq, Ord)

-- Map items to their base type
itemTypes :: Map Item ItemType
itemTypes =
  Map.fromList
    [ (RustySword, Sword)
    , (SilverSword, Sword)
    , (BrassLantern, Lantern)
    , (WaterFlask, Flask)
    , (EmptyFlask, Flask)
    ]

-- Map items to their adjectives
itemAdjectives :: Map Item (Set Adjective)
itemAdjectives =
  Map.fromList
    [ (RustySword, Set.fromList [Rusty, Metal])
    , (SilverSword, Set.fromList [Silver, Metal])
    , (BrassLantern, Set.fromList [Brass, Metal])
    , (WaterFlask, Set.fromList [Water])
    , (EmptyFlask, Set.fromList [Empty])
    ]

-- Base type names
typeNames :: Map ItemType String
typeNames =
  Map.fromList
    [ (Sword, "sword")
    , (Lantern, "lantern")
    , (Flask, "flask")
    ]

-- Adjective names (for display and matching)
adjectiveNames :: Map Adjective String
adjectiveNames =
  Map.fromList
    [ (Rusty, "rusty")
    , (Silver, "silver")
    , (Brass, "brass")
    , (Metal, "metal")
    , (Water, "water")
    , (Empty, "empty")
    ]

-- Item descriptions
itemDescriptions :: Map Item String
itemDescriptions =
  Map.fromList
    [ (RustySword, "An old rusty sword. It's not much, but it's better than nothing.")
    , (SilverSword, "A finely crafted silver sword. It gleams in the light.")
    , (BrassLantern, "A well-crafted brass lantern. It could help light up dark places.")
    , (WaterFlask, "A flask filled with clear water.")
    , (EmptyFlask, "An empty flask that could be used to carry liquids.")
    ]

-- Helper functions
getItemName :: Item -> String
getItemName item =
  let
    adjectives = maybe [] Set.toList $ Map.lookup item itemAdjectives
    adjectiveStrs = map (\adj -> Map.findWithDefault "unknown" adj adjectiveNames) adjectives
    baseType = Map.findWithDefault Sword item itemTypes
    baseStr = Map.findWithDefault "unknown" baseType typeNames
   in
    unwords $ filter (/= "metal") adjectiveStrs ++ [baseStr]

getItemDescription :: Item -> String
getItemDescription item = Map.findWithDefault "No description available." item itemDescriptions

getItemType :: Item -> ItemType
getItemType item = Map.findWithDefault Sword item itemTypes

getItemAdjectives :: Item -> Set Adjective
getItemAdjectives item = Map.findWithDefault Set.empty item itemAdjectives

-- Convert a string to a possible adjective
parseAdjective :: String -> Maybe Adjective
parseAdjective str =
  let
    strLower = map toLower str
   in
    fst <$> find (\(_, name) -> strLower == name) (Map.toList adjectiveNames)

-- Match an item against a type and list of adjectives
matchesTypeAndAdjectives :: ItemType -> [Adjective] -> Item -> Bool
matchesTypeAndAdjectives reqType reqAdjs item =
  let
    itemType = getItemType item
    itemAdjs = getItemAdjectives item
   in
    itemType == reqType && all (`Set.member` itemAdjs) reqAdjs

-- Parse and match items based on user input
-- Returns Either an error message or a list of matching items
matchItem :: String -> [Item] -> Either String [Item]
matchItem input items =
  let
    words' = words $ map toLower input
   in
    if null words'
      then Left "Please specify an item."
      else
        let
          -- Last word should be the type, rest are potential adjectives
          (adjStrs, typeStr) = (init words', last words')
          -- Try to parse all adjectives
          adjectives = mapMaybe parseAdjective adjStrs
          -- Try to find matching type
          matchingType = fst <$> find (\(_, name) -> typeStr == name) (Map.toList typeNames)
         in
          case matchingType of
            Nothing -> Left $ "I don't know what '" ++ typeStr ++ "' is."
            Just itemType ->
              let matches = filter (matchesTypeAndAdjectives itemType adjectives) items
               in case matches of
                    [] -> Left $ "I can't find any " ++ input ++ " here."
                    _ -> Right matches