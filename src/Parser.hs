{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Parser (
  parseCommand,
) where

import Commands (Command (..))
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

-- Direction parser
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

look :: Parser Command
look =
  Look <$ (word "look" <|> word "l")

examine :: Parser Command
examine =
  Examine
    <$ choice [lookat, examine']
    <*> many adjective
    <*> itemType'
 where
  lookat = word "look" >> optword "at" >> article
  examine' = word "examine" >> article

inventory :: Parser Command
inventory =
  Inventory <$ (word "inventory" <|> word "i")

take_ :: Parser Command
take_ = take' <|> get <|> pickup
 where
  take' = try $ Take <$ word "take" <* optional' article <*> many adjective <*> itemType'
  get = try $ Take <$ word "get" <* optional' article <*> many adjective <*> itemType'
  pickup =
    choice
      [ try $ Take <$ word "pick" <* word "up" <* optional' article <*> many adjective <*> itemType'
      , try $ Take <$ word "pick" <* optional' article <*> many adjective <*> itemType' <* word "up"
      ]

drop_ :: Parser Command
drop_ = drop' <|> put
 where
  drop' = try $ Drop <$ word "drop" <* optional' article <*> many adjective <*> itemType'
  put =
    choice
      [ try $ Drop <$ word "put" <* optional' (word "down") <* optional' article <*> many adjective <*> itemType'
      , try $ Drop <$ word "put" <* optional' article <*> many adjective <*> itemType' <* word "down"
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