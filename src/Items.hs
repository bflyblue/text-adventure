module Items (
  Item (..),
  getItemName,
  getItemDescription,
) where

import Data.Map (Map)
import qualified Data.Map as Map

-- Item type
data Item
  = RustySword
  | BrassLantern
  | WaterFlask
  deriving (Show, Eq, Ord)

-- Item names
itemNames :: Map Item String
itemNames =
  Map.fromList
    [ (RustySword, "rusty sword")
    , (BrassLantern, "brass lantern")
    , (WaterFlask, "water flask")
    ]

-- Item descriptions
itemDescriptions :: Map Item String
itemDescriptions =
  Map.fromList
    [ (RustySword, "An old rusty sword. It's not much, but it's better than nothing.")
    , (BrassLantern, "A well-crafted brass lantern. It could help light up dark places.")
    , (WaterFlask, "An empty flask that could be used to carry water.")
    ]

-- Helper functions
getItemName :: Item -> String
getItemName item = Map.findWithDefault "Unnamed Item" item itemNames

getItemDescription :: Item -> String
getItemDescription item = Map.findWithDefault "No description available." item itemDescriptions