{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Main where

import Commands (Command (..), CommandResult (..))
import Control.Monad (unless)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GameState (GameState (..))
import Items (Item (..), adjectiveName, itemName, itemTypeName, matchItem)
import Parser (parseCommand)
import Rooms (
  Room (..),
  getRoomDescription,
  getRoomExits,
  getRoomName,
 )
import System.IO (hFlush, stdout)
import Types (dirName)

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
          else "You are carrying: " <> T.intercalate ", " (map itemName (inventory state))
  Take adjs itemType ->
    case matchItem adjs itemType (getRoomItems (currentRoom state) state) of
      [] ->
        pure $ CommandResult state $ "You don't see " <> T.intercalate ", " (map adjectiveName adjs) <> " " <> itemTypeName itemType <> "."
      [item] ->
        pure $
          CommandResult
            ( state
                { inventory = item : inventory state
                , roomItems = Map.adjust (filter (/= item)) (currentRoom state) (roomItems state)
                }
            )
            ("You take the " <> itemName item <> ".")
      multipleItems -> do
        let msg = T.unlines $ ("Which " <> itemTypeName itemType <> " do you mean?") : map (\i -> "- " <> itemName i) multipleItems
        pure $ CommandResult state msg
  Drop adjs itemType ->
    case matchItem adjs itemType (inventory state) of
      [] ->
        pure $ CommandResult state $ "You don't see " <> T.intercalate ", " (map adjectiveName adjs) <> " " <> itemTypeName itemType <> "."
      [item] ->
        pure $
          CommandResult
            ( state
                { inventory = filter (/= item) (inventory state)
                , roomItems = Map.adjust (item :) (currentRoom state) (roomItems state)
                }
            )
            ("You drop the " <> itemName item <> ".")
      multipleItems -> do
        let msg = T.unlines $ ("Which " <> itemTypeName itemType <> " do you mean?") : map (\i -> "- " <> itemName i) multipleItems
        pure $ CommandResult state msg
  Move dir ->
    pure $ case Map.lookup dir (getRoomExits (currentRoom state)) of
      Nothing -> CommandResult state $ "You can't go " <> dirName dir <> "."
      Just newRoom ->
        CommandResult
          (state{currentRoom = newRoom})
          ("You move " <> dirName dir <> ".")
  Help -> pure $ CommandResult state "Type 'help' for a list of commands."
  Quit -> pure $ CommandResult state "Goodbye!"

-- Display current game state
displayState :: GameState -> IO ()
displayState state = do
  let room = currentRoom state
  TIO.putStrLn $ "\n" <> getRoomName room
  TIO.putStrLn $ getRoomDescription room
  let items = getRoomItems room state
  unless (null items) $
    TIO.putStrLn $
      "You see: " <> T.intercalate ", " (map itemName items)
  let exits = Map.keys $ getRoomExits room
  unless (null exits) $
    TIO.putStrLn $
      "Exits: " <> T.intercalate ", " (map dirName exits)

-- Main game loop
gameLoop :: GameState -> IO ()
gameLoop state = do
  TIO.putStr "> "
  hFlush stdout
  input <- TIO.getLine
  case parseCommand input of
    Left err -> do
      TIO.putStrLn $ "I don't understand that command: " <> err
      TIO.putStrLn "Type 'help' for a list of commands."
      gameLoop state
    Right cmd -> case cmd of
      Quit -> TIO.putStrLn "Thanks for playing!"
      Help -> do
        TIO.putStrLn "Available commands:"
        TIO.putStrLn "  look                    - Look around the current room (or 'l')"
        TIO.putStrLn "  inventory               - Check your inventory (or 'i')"
        TIO.putStrLn "  take/get [item]        - Pick up an item"
        TIO.putStrLn "  drop [item]            - Drop an item"
        TIO.putStrLn "  go north/south/etc     - Move in a direction"
        TIO.putStrLn "  north/south/east/west  - Move in a direction (or n/s/e/w)"
        TIO.putStrLn "  quit                   - Exit the game (or 'exit', 'q')"
        TIO.putStrLn "  help                   - Show this help message"
        TIO.putStrLn ""
        TIO.putStrLn "Notes:"
        TIO.putStrLn "- Articles (a/an/the) are optional"
        TIO.putStrLn "- Items can be referred to by their full name or base name"
        TIO.putStrLn "  (e.g. 'take rusty sword' or just 'take sword')"
        TIO.putStrLn "- You can use shorter directions (n/s/e/w)"
        TIO.putStrLn "- 'pick up' works the same as 'take'"
        gameLoop state
      _ -> do
        result <- handleCommand cmd state
        TIO.putStrLn $ message result
        displayState (newState result)
        gameLoop (newState result)

main :: IO ()
main = do
  TIO.putStrLn "Welcome to the Text Adventure!"
  TIO.putStrLn "Type 'help' for a list of commands."
  TIO.putStrLn ""
  displayState initialState
  gameLoop initialState