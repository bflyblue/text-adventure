{-# LANGUAGE DerivingStrategies #-}

module Game.Monad where

import Control.Monad.State.Strict
import Data.Functor ((<&>))
import Game.Actions qualified as A
import Game.State
import Items (Item)
import Rooms (Room)

newtype Game a = Game (StateT GameState IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState GameState)

runGame :: GameState -> Game a -> IO a
runGame s (Game m) = evalStateT m s

getRoomItems :: Room -> Game [Item]
getRoomItems room = get <&> A.getRoomItems room

takeItemFromRoom :: Room -> Item -> Game ()
takeItemFromRoom room item = modify (A.takeItemFromRoom room item)

dropItemToRoom :: Room -> Item -> Game ()
dropItemToRoom room item = modify (A.dropItemToRoom room item)

addItemToRoom :: Room -> Item -> Game ()
addItemToRoom room item = modify (A.addItemToRoom room item)

removeItemFromRoom :: Room -> Item -> Game ()
removeItemFromRoom room item = modify (A.removeItemFromRoom room item)

addItemToInventory :: Item -> Game ()
addItemToInventory item = modify (A.addItemToInventory item)

removeItemFromInventory :: Item -> Game ()
removeItemFromInventory item = modify (A.removeItemFromInventory item)

moveToRoom :: Room -> Game ()
moveToRoom room = modify (A.moveToRoom room)
