{-# LANGUAGE OverloadedStrings #-}

module Parser (
  parseCommand,
) where

import Commands (Command (..))
import Control.Monad (void, when)
import Data.Char (isSpace, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types (Direction (..))

type Parser = Parsec Void Text

-- Parser utilities
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

-- Parse a word that isn't a reserved word
word :: Parser Text
word = lexeme $ takeWhile1P (Just "word") (\c -> not (isSpace c))

-- Parse articles (a, an, the) and throw them away
article :: Parser ()
article =
  void $
    choice
      [ symbol "a"
      , symbol "an"
      , symbol "the"
      ]

-- Optional parser
optional' :: Parser a -> Parser ()
optional' p = void $ optional p

-- Direction parser
direction :: Parser Direction
direction =
  choice
    [ North <$ (symbol "north" <|> symbol "n")
    , South <$ (symbol "south" <|> symbol "s")
    , East <$ (symbol "east" <|> symbol "e")
    , West <$ (symbol "west" <|> symbol "w")
    ]

-- Item name parser (allows multiple words)
parseItemName :: Parser String
parseItemName = T.unpack . T.unwords <$> some word

-- Command parser
command :: Parser Command
command =
  choice
    [ Look <$ (symbol "look" <|> symbol "l")
    , Inventory <$ (symbol "inventory" <|> symbol "i")
    , try $ do
        verb <- symbol "take" <|> symbol "get" <|> symbol "pick"
        when (verb == "pick") $ void $ symbol "up"
        optional' article
        Take <$> parseItemName
    , try $ do
        void $ symbol "drop" <|> symbol "put"
        optional' $ symbol "down"
        optional' article
        Drop <$> parseItemName
    , try $ do
        void $ symbol "go" <|> symbol "move"
        Move <$> direction
    , Move <$> direction
    , Help <$ symbol "help"
    , Quit <$ (symbol "quit" <|> symbol "exit" <|> symbol "q")
    ]

-- Parse user input into commands
parseCommand :: String -> Either String Command
parseCommand input = case parse (space *> command <* eof) "" (T.pack $ map toLower input) of
  Left err -> Left $ errorBundlePretty err
  Right cmd -> Right cmd