{-# LANGUAGE StrictData #-}

module Main where

import Commands (Command (..), CommandResult (..))
import Control.Monad (unless)
import Data.Char (toLower)
import Data.List (find, intercalate, isInfixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import GameState (GameState (..))
import Items (Item (..), getItemName)
import Parser (parseCommand)
import Rooms (
  Room (..),
  getRoomDescription,
  getRoomExits,
  getRoomName,
 )
import System.IO (hFlush, stdout)

-- Initial room items configuration
initialRoomItems :: Map Room [Item]
initialRoomItems =
  Map.fromList
    [ (ForestClearing, [RustySword])
    , (AbandonedCabin, [BrassLantern])
    , (RiverBank, [WaterFlask])
    ]

-- Initial game state
initialState :: GameState
initialState =
  GameState
    { currentRoom = ForestClearing
    , inventory = []
    , roomItems = initialRoomItems
    }

-- Helper functions for room state
getRoomItems :: Room -> GameState -> [Item]
getRoomItems room state = Map.findWithDefault [] room (roomItems state)

setRoomItems :: Room -> [Item] -> GameState -> GameState
setRoomItems room items state =
  state{roomItems = Map.insert room items (roomItems state)}

-- Helper function to find an item by name
findItem :: String -> [Item] -> Maybe Item
findItem name = find (\item -> map toLower name `isInfixOf` map toLower (getItemName item))

-- Handle game commands
handleCommand :: Command -> GameState -> CommandResult
handleCommand cmd state = case cmd of
  Look -> CommandResult state "You look around."
  Inventory ->
    CommandResult state $
      if null (inventory state)
        then "Your inventory is empty."
        else "You are carrying: " ++ intercalate ", " (map getItemName $ inventory state)
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
          ("You take the " ++ getItemName item ++ ".")
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
          ("You drop the " ++ getItemName item ++ ".")
  Move dir -> case Map.lookup dir (getRoomExits (currentRoom state)) of
    Nothing -> CommandResult state $ "You can't go " ++ show dir ++ "."
    Just newRoom ->
      CommandResult
        (state{currentRoom = newRoom})
        ("You move " ++ show dir ++ ".")
  Help -> CommandResult state "Type 'help' for a list of commands."
  Quit -> CommandResult state "Goodbye!"

-- Display current game state
displayState :: GameState -> IO ()
displayState state = do
  let room = currentRoom state
  putStrLn $ "\n" ++ getRoomName room
  putStrLn $ getRoomDescription room
  let items = getRoomItems room state
  unless (null items) $
    putStrLn $
      "You see: " ++ intercalate ", " (map getItemName items)
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
    Left err -> do
      putStrLn $ "I don't understand that command: " ++ err
      putStrLn "Type 'help' for a list of commands."
      gameLoop state
    Right cmd -> case cmd of
      Quit -> putStrLn "Thanks for playing!"
      Help -> do
        putStrLn "Available commands:"
        putStrLn "  look                    - Look around the current room (or 'l')"
        putStrLn "  inventory               - Check your inventory (or 'i')"
        putStrLn "  take/get [item]        - Pick up an item"
        putStrLn "  drop [item]            - Drop an item"
        putStrLn "  go north/south/etc     - Move in a direction"
        putStrLn "  north/south/east/west  - Move in a direction (or n/s/e/w)"
        putStrLn "  quit                   - Exit the game (or 'exit', 'q')"
        putStrLn "  help                   - Show this help message"
        putStrLn ""
        putStrLn "Notes:"
        putStrLn "- Articles (a/an/the) are optional"
        putStrLn "- Items can have multiple words (e.g. 'take rusty sword')"
        putStrLn "- You can use shorter directions (n/s/e/w)"
        putStrLn "- 'pick up' works the same as 'take'"
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