{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Main where

import Commands (Command (..), CommandResult (..), ItemSelection (..))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (gets)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Game.Initial (initialState)
import Game.Monad (Game, dropItemToRoom, getRoomItems, moveToRoom, runGame, takeItemFromRoom)
import Game.State (GameState (..))
import Items (Item, adjectiveName, itemName, itemTypeName, matchItem)
import Parser (parseCommand)
import Rooms (
  roomDescription,
  roomExits,
  roomName,
 )
import System.IO (hFlush, stdout)
import Types (dirName)

-- Handle game commands
handleCommand :: Command -> Game CommandResult
handleCommand cmd = do
  case cmd of
    Look -> pure $ CommandResult "You look around."
    Inventory -> do
      inventory' <- gets inventory
      pure $
        CommandResult $
          if null inventory'
            then "Your inventory is empty."
            else "You are carrying: " <> T.intercalate ", " (map itemName inventory')
    Take itemSelection -> do
      singleItemAction itemSelection $ \item -> do
        room <- gets currentRoom
        takeItemFromRoom room item
        pure $ CommandResult $ "You take the " <> itemName item <> "."
    Drop itemSelection -> do
      singleItemAction itemSelection $ \item -> do
        room <- gets currentRoom
        dropItemToRoom room item
        pure $ CommandResult $ "You drop the " <> itemName item <> "."
    Move dir -> do
      room <- gets currentRoom
      case Map.lookup dir (roomExits room) of
        Nothing -> pure $ CommandResult $ "You can't go " <> dirName dir <> "."
        Just newRoom -> do
          moveToRoom newRoom
          pure $ CommandResult $ "You move " <> dirName dir <> "."
    Help -> pure $ CommandResult "Type 'help' for a list of commands."
    Quit -> pure $ CommandResult "Goodbye!"
    _ -> pure $ CommandResult "Unknown command."

singleItemAction :: ItemSelection -> (Item -> Game CommandResult) -> Game CommandResult
singleItemAction (ItemSelection adjs itemType) action = do
  room <- gets currentRoom
  roomItems <- getRoomItems room
  case matchItem adjs itemType roomItems of
    [] ->
      pure $ CommandResult $ "You don't see " <> T.intercalate ", " (map adjectiveName adjs) <> " " <> itemTypeName itemType <> "."
    [item] -> action item
    multipleItems -> do
      pure $ CommandResult $ T.unlines $ ("Which " <> itemTypeName itemType <> " do you mean?") : map (\i -> "- " <> itemName i) multipleItems

write :: T.Text -> Game ()
write = liftIO . TIO.putStrLn

displayState :: Game ()
displayState = do
  room <- gets currentRoom
  write $ roomName room
  write $ roomDescription room
  items <- getRoomItems room
  unless (null items) $
    write $
      "You see: " <> T.intercalate ", " (map itemName items)
  let exits = Map.keys $ roomExits room
  unless (null exits) $
    write $
      "Exits: " <> T.intercalate ", " (map dirName exits)

gameLoop :: Game ()
gameLoop = do
  displayState
  liftIO $ TIO.putStr "> " >> hFlush stdout
  input <- liftIO TIO.getLine
  case parseCommand input of
    Left err -> do
      write $ "I don't understand that command: " <> err
      write "Type 'help' for a list of commands."
      gameLoop
    Right cmd -> case cmd of
      Quit -> write "Thanks for playing!"
      Help -> do
        write "Available commands:"
        write "  look                   - Look around the current room (or 'l')"
        write "  inventory              - Check your inventory (or 'i')"
        write "  take/get [item]        - Pick up an item"
        write "  drop [item]            - Drop an item"
        write "  go north/south/etc     - Move in a direction"
        write "  north/south/east/west  - Move in a direction (or n/s/e/w)"
        write "  quit                   - Exit the game (or 'exit', 'q')"
        write "  help                   - Show this help message"
        write ""
        write "Notes:"
        write "- Articles (a/an/the) are optional"
        write "- Items can be referred to by their full name or base name"
        write "  (e.g. 'take rusty sword' or just 'take sword')"
        write "- You can use shorter directions (n/s/e/w)"
        write "- 'pick up' works the same as 'take'"
        gameLoop
      _ -> do
        result <- handleCommand cmd
        write $ message result
        gameLoop

start :: Game ()
start = do
  write "Welcome to the Text Adventure!"
  write "Type 'help' for a list of commands."
  write ""
  gameLoop

main :: IO ()
main = runGame initialState start