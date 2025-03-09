{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Parser (
  parseCommand,
) where

import Commands (Command (..))
import Control.Monad (void, when)
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

-- Command parser
command :: Parser Command
command =
  choice
    [ Look <$ (word "look" <|> word "l")
    , Inventory <$ (word "inventory" <|> word "i")
    , try $ do
        verb <- word "take" <|> word "get" <|> word "pick"
        when (verb == "pick") $ void $ word "up"
        optional' article
        Take <$> many adjective <*> itemType'
    , try $ do
        void $ word "drop" <|> word "put"
        optional' $ word "down"
        optional' article
        Drop <$> many adjective <*> itemType'
    , try $ do
        void $ word "go" <|> word "move"
        Move <$> direction
    , Move <$> direction
    , Help <$ word "help"
    , Quit <$ (word "quit" <|> word "exit" <|> word "q")
    ]

-- Parse user input into commands
parseCommand :: Text -> Either Text Command
parseCommand input = case parse (space *> command <* eof) "" (T.toLower input) of
  Left err -> Left $ T.pack $ errorBundlePretty err
  Right cmd -> Right cmd