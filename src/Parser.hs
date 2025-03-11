{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Parser (
  parseCommand,
) where

import Commands (Command (..), ItemSelection (..))
import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Items
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Types (Direction (..))

type Parser = Parsec Void Text

-- Match a word
word :: Text -> Parser Text
word = L.symbol space

-- Parse articles (a, an, the) and throw them away
article :: Parser ()
article =
  void $
    choice
      [ word "a"
      , word "an"
      , word "the"
      ]

-- Optional parser
optional' :: Parser a -> Parser ()
optional' p = void $ optional p

optword :: Text -> Parser ()
optword = optional' . word

direction :: Parser Direction
direction =
  choice
    [ North <$ (word "north" <|> word "n")
    , South <$ (word "south" <|> word "s")
    , East <$ (word "east" <|> word "e")
    , West <$ (word "west" <|> word "w")
    ]

enumerate :: (Enum a, Bounded a) => (a -> Text) -> Parser a
enumerate f =
  choice $ map (\x -> try $ x <$ word (f x)) [minBound .. maxBound]

adjective :: Parser Adjective
adjective = enumerate adjectiveName

itemType' :: Parser ItemType
itemType' = enumerate itemTypeName

itemSelection :: Parser ItemSelection
itemSelection =
  ItemSelection
    <$> many adjective
    <*> itemType'

look :: Parser Command
look =
  Look <$ (word "look" <|> word "l")

examine :: Parser Command
examine =
  Examine
    <$ choice [lookat, examine']
    <*> itemSelection
 where
  lookat = word "look" >> optword "at" >> article
  examine' = word "examine" >> article

inventory :: Parser Command
inventory =
  Inventory <$ (word "inventory" <|> word "i")

take_ :: Parser Command
take_ = take' <|> get <|> pickup
 where
  take' = try $ Take <$ word "take" <* optional' article <*> itemSelection
  get = try $ Take <$ word "get" <* optional' article <*> itemSelection
  pickup =
    choice
      [ try $ Take <$ word "pick" <* word "up" <* optional' article <*> itemSelection
      , try $ Take <$ word "pick" <* optional' article <*> itemSelection <* word "up"
      ]

drop_ :: Parser Command
drop_ = drop' <|> put
 where
  drop' = try $ Drop <$ word "drop" <* optional' article <*> itemSelection
  put =
    choice
      [ try $ Drop <$ word "put" <* optional' (word "down") <* optional' article <*> itemSelection
      , try $ Drop <$ word "put" <* optional' article <*> itemSelection <* word "down"
      ]

move :: Parser Command
move = move' <|> direction'
 where
  move' = Move <$ (word "go" <|> word "move") <*> direction
  direction' = Move <$> direction

help :: Parser Command
help = Help <$ word "help"

quit :: Parser Command
quit = Quit <$ (word "quit" <|> word "exit" <|> word "q")

-- Command parser
command :: Parser Command
command =
  choice
    [ look
    , examine
    , inventory
    , take_
    , drop_
    , move
    , help
    , quit
    ]

-- Parse user input into commands
parseCommand :: Text -> Either Text Command
parseCommand input = case parse (space *> command <* eof) "" (T.toLower input) of
  Left err -> Left $ T.pack $ errorBundlePretty err
  Right cmd -> Right cmd