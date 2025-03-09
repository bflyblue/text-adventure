{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Main where

import Commands (Command (..), CommandResult (..))
import Control.Monad (unless)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GameState (GameState (..), dropItemToRoom, getRoomItems, initialState, takeItemFromRoom)
import Items (adjectiveName, itemName, itemTypeName, matchItem)
import Parser (parseCommand)
import Rooms (
  roomDescription,
  roomExits,
  roomName,
 )
import System.IO (hFlush, stdout)
import Types (dirName)

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
            (takeItemFromRoom (currentRoom state) item state)
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
            (dropItemToRoom (currentRoom state) item state)
            ("You drop the " <> itemName item <> ".")
      multipleItems -> do
        let msg = T.unlines $ ("Which " <> itemTypeName itemType <> " do you mean?") : map (\i -> "- " <> itemName i) multipleItems
        pure $ CommandResult state msg
  Move dir ->
    pure $ case Map.lookup dir (roomExits (currentRoom state)) of
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
  TIO.putStrLn $ "\n" <> roomName room
  TIO.putStrLn $ roomDescription room
  let items = getRoomItems room state
  unless (null items) $
    TIO.putStrLn $
      "You see: " <> T.intercalate ", " (map itemName items)
  let exits = Map.keys $ roomExits room
  unless (null exits) $
    TIO.putStrLn $
      "Exits: " <> T.intercalate ", " (map dirName exits)

-- Main game loop
gameLoop :: GameState -> IO ()
gameLoop state = do
  displayState state
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
        TIO.putStrLn "  look                   - Look around the current room (or 'l')"
        TIO.putStrLn "  inventory              - Check your inventory (or 'i')"
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
        gameLoop (newState result)

main :: IO ()
main = do
  TIO.putStrLn "Welcome to the Text Adventure!"
  TIO.putStrLn "Type 'help' for a list of commands."
  TIO.putStrLn ""
  gameLoop initialState