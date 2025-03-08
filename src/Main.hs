{-# LANGUAGE StrictData #-}

module Main where

import Control.Monad (unless)
import Data.Char (toLower)
import Data.List (find, intercalate, isInfixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import System.IO (hFlush, stdout)

-- Data types
data Item = Item
  { itemName :: String
  , itemDescription :: String
  }
  deriving (Show, Eq)

-- Room is now a sum type
data Room
  = ForestClearing
  | AbandonedCabin
  | RiverBank
  deriving (Show, Eq, Ord)

data Direction = North | South | East | West
  deriving (Show, Eq, Ord)

data GameState = GameState
  { currentRoom :: Room
  , inventory :: [Item]
  , roomItems :: Map Room [Item]
  }
  deriving (Show)

-- Room descriptions
roomDescriptions :: Map Room String
roomDescriptions =
  Map.fromList
    [ (ForestClearing, "You are in a peaceful forest clearing. Sunlight filters through the leaves above.")
    , (AbandonedCabin, "An old wooden cabin stands here, its door hanging loose on rusty hinges.")
    , (RiverBank, "A gentle river flows here. The water looks cool and refreshing.")
    ]

-- Room names
roomNames :: Map Room String
roomNames =
  Map.fromList
    [ (ForestClearing, "Forest Clearing")
    , (AbandonedCabin, "Abandoned Cabin")
    , (RiverBank, "River Bank")
    ]

-- Initial room items configuration
initialRoomItems :: Map Room [Item]
initialRoomItems =
  Map.fromList
    [ (ForestClearing, [sword])
    , (AbandonedCabin, [lantern])
    , (RiverBank, [flask])
    ]

-- Room exits
roomExits :: Map Room (Map Direction Room)
roomExits =
  Map.fromList
    [ (ForestClearing, Map.fromList [(North, AbandonedCabin), (East, RiverBank)])
    , (AbandonedCabin, Map.fromList [(South, ForestClearing)])
    , (RiverBank, Map.fromList [(West, ForestClearing)])
    ]

-- Initial game state
initialState :: GameState
initialState =
  GameState
    { currentRoom = ForestClearing
    , inventory = []
    , roomItems = initialRoomItems
    }

-- Items
lantern :: Item
lantern =
  Item
    { itemName = "brass lantern"
    , itemDescription = "A well-crafted brass lantern. It could help light up dark places."
    }

flask :: Item
flask =
  Item
    { itemName = "water flask"
    , itemDescription = "An empty flask that could be used to carry water."
    }

sword :: Item
sword =
  Item
    { itemName = "rusty sword"
    , itemDescription = "An old rusty sword. It's not much, but it's better than nothing."
    }

-- Game commands
data Command
  = Look
  | Inventory
  | Take String
  | Drop String
  | Move Direction
  | Help
  | Quit
  deriving (Show)

-- Parse user input into commands
parseCommand :: String -> Maybe Command
parseCommand input = case words (map toLower input) of
  ["look"] -> Just Look
  ["l"] -> Just Look
  ["inventory"] -> Just Inventory
  ["i"] -> Just Inventory
  ["take", item] -> Just $ Take item
  ["get", item] -> Just $ Take item
  ["drop", item] -> Just $ Drop item
  ["north"] -> Just $ Move North
  ["n"] -> Just $ Move North
  ["south"] -> Just $ Move South
  ["s"] -> Just $ Move South
  ["east"] -> Just $ Move East
  ["e"] -> Just $ Move East
  ["west"] -> Just $ Move West
  ["w"] -> Just $ Move West
  ["help"] -> Just Help
  ["quit"] -> Just Quit
  _ -> Nothing

-- Game commands with results
data CommandResult = CommandResult
  { newState :: GameState
  , message :: String
  }

-- Helper functions for room state
getRoomItems :: Room -> GameState -> [Item]
getRoomItems room state = Map.findWithDefault [] room (roomItems state)

setRoomItems :: Room -> [Item] -> GameState -> GameState
setRoomItems room items state =
  state{roomItems = Map.insert room items (roomItems state)}

getRoomExits :: Room -> Map Direction Room
getRoomExits room = Map.findWithDefault Map.empty room roomExits

getRoomDescription :: Room -> String
getRoomDescription room = Map.findWithDefault "You are in an undefined space." room roomDescriptions

getRoomName :: Room -> String
getRoomName room = Map.findWithDefault "Unnamed Room" room roomNames

-- Handle game commands
handleCommand :: Command -> GameState -> CommandResult
handleCommand cmd state = case cmd of
  Look -> CommandResult state "You look around."
  Inventory ->
    CommandResult state $
      if null (inventory state)
        then "Your inventory is empty."
        else "You are carrying: " ++ intercalate ", " (map itemName $ inventory state)
  Take itemName -> case findItem itemName (getRoomItems (currentRoom state) state) of
    Nothing -> CommandResult state $ "There is no " ++ itemName ++ " here."
    Just item ->
      let
        oldItems = getRoomItems (currentRoom state) state
        newItems = filter (/= item) oldItems
        newState = setRoomItems (currentRoom state) newItems state
        finalState = newState{inventory = item : inventory state}
       in
        CommandResult
          finalState
          ("You take the " ++ itemName ++ ".")
  Drop itemName -> case findItem itemName (inventory state) of
    Nothing -> CommandResult state $ "You don't have a " ++ itemName ++ "."
    Just item ->
      let
        oldItems = getRoomItems (currentRoom state) state
        newItems = item : oldItems
        newState = setRoomItems (currentRoom state) newItems state
        finalState = newState{inventory = filter (/= item) (inventory state)}
       in
        CommandResult
          finalState
          ("You drop the " ++ itemName ++ ".")
  Move dir -> case Map.lookup dir (getRoomExits (currentRoom state)) of
    Nothing -> CommandResult state $ "You can't go " ++ show dir ++ "."
    Just newRoom ->
      CommandResult
        (state{currentRoom = newRoom})
        ("You move " ++ show dir ++ ".")
  Help -> CommandResult state "Type 'help' for a list of commands."
  Quit -> CommandResult state "Goodbye!"

-- Helper function to find an item by name
findItem :: String -> [Item] -> Maybe Item
findItem name = find (\item -> map toLower name `isInfixOf` map toLower (itemName item))

-- Display current game state
displayState :: GameState -> IO ()
displayState state = do
  let room = currentRoom state
  putStrLn $ "\n" ++ getRoomName room
  putStrLn $ getRoomDescription room
  let items = getRoomItems room state
  unless (null items) $
    putStrLn $
      "You see: " ++ intercalate ", " (map itemName items)
  let exits = Map.keys $ getRoomExits room
  unless (null exits) $
    putStrLn $
      "Exits: " ++ intercalate ", " (map show exits)

-- Main game loop
gameLoop :: GameState -> IO ()
gameLoop state = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case parseCommand input of
    Nothing -> do
      putStrLn "I don't understand that command. Type 'help' for a list of commands."
      gameLoop state
    Just cmd -> case cmd of
      Quit -> putStrLn "Thanks for playing!"
      Help -> do
        putStrLn "Available commands:"
        putStrLn "  look        - Look around the current room"
        putStrLn "  inventory   - Check your inventory"
        putStrLn "  take [item] - Pick up an item"
        putStrLn "  drop [item] - Drop an item"
        putStrLn "  north       - Move north (also: n)"
        putStrLn "  south       - Move south (also: s)"
        putStrLn "  east        - Move east  (also: e)"
        putStrLn "  west        - Move west  (also: w)"
        putStrLn "  quit        - Exit the game"
        putStrLn "  help        - Show this help message"
        gameLoop state
      _ -> do
        let result = handleCommand cmd state
        putStrLn $ message result
        displayState (newState result)
        gameLoop (newState result)

main :: IO ()
main = do
  putStrLn "Welcome to the Text Adventure!"
  putStrLn "Type 'help' for a list of commands."
  putStrLn ""
  displayState initialState
  gameLoop initialState