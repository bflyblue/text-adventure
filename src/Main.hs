{-# LANGUAGE StrictData #-}

module Main where

import Commands (Command (..), CommandResult (..))
import Control.Monad (unless)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import GameState (GameState (..))
import Items (Item (..), getItemName, matchItem)
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
    [ (ForestClearing, [RustySword, SilverSword])
    , (AbandonedCabin, [BrassLantern])
    , (RiverBank, [WaterFlask, EmptyFlask])
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

-- Handle game commands
handleCommand :: Command -> GameState -> IO CommandResult
handleCommand cmd state = case cmd of
  Look -> pure $ CommandResult state "You look around."
  Inventory ->
    pure $
      CommandResult state $
        if null (inventory state)
          then "Your inventory is empty."
          else "You are carrying: " ++ intercalate ", " (map getItemName $ inventory state)
  Take itemName ->
    case matchItem itemName (getRoomItems (currentRoom state) state) of
      Left err ->
        pure $ CommandResult state err
      Right [item] ->
        pure $
          CommandResult
            ( state
                { inventory = item : inventory state
                , roomItems = Map.adjust (filter (/= item)) (currentRoom state) (roomItems state)
                }
            )
            ("You take the " ++ getItemName item ++ ".")
      Right matches -> do
        putStrLn $ "Which " ++ itemName ++ " do you mean?"
        mapM_ (\item -> putStrLn $ "- " ++ getItemName item) matches
        pure $ CommandResult state "Please be more specific."
  Drop itemName ->
    case matchItem itemName (inventory state) of
      Left err ->
        pure $ CommandResult state err
      Right [item] ->
        pure $
          CommandResult
            ( state
                { inventory = filter (/= item) (inventory state)
                , roomItems = Map.adjust (item :) (currentRoom state) (roomItems state)
                }
            )
            ("You drop the " ++ getItemName item ++ ".")
      Right matches -> do
        putStrLn $ "Which " ++ itemName ++ " do you mean?"
        mapM_ (\item -> putStrLn $ "- " ++ getItemName item) matches
        pure $ CommandResult state "Please be more specific."
  Move dir ->
    pure $ case Map.lookup dir (getRoomExits (currentRoom state)) of
      Nothing -> CommandResult state $ "You can't go " ++ show dir ++ "."
      Just newRoom ->
        CommandResult
          (state{currentRoom = newRoom})
          ("You move " ++ show dir ++ ".")
  Help -> pure $ CommandResult state "Type 'help' for a list of commands."
  Quit -> pure $ CommandResult state "Goodbye!"

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
        putStrLn "- Items can be referred to by their full name or base name"
        putStrLn "  (e.g. 'take rusty sword' or just 'take sword')"
        putStrLn "- You can use shorter directions (n/s/e/w)"
        putStrLn "- 'pick up' works the same as 'take'"
        gameLoop state
      _ -> do
        result <- handleCommand cmd state
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