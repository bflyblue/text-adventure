{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Ollama where

import Data.Aeson (Value)
import Data.Map (Map)
import Data.Text (Text)

type JsonValue = Value

data Role = SystemRole | UserRole | AssistantRole | ToolRole deriving (Show, Eq)

newtype Tool = Tool
  { function :: Function
  }

data Function = Function
  { name :: Text
  , description :: Text
  , parameters :: JsonSchema
  }

data JsonSchema
  = ObjectSchema
      { properties :: Map Text JsonSchema
      , description :: Maybe Text
      , required :: [Text]
      }
  | ArraySchema
      { items :: JsonSchema
      , description :: Maybe Text
      , minItems :: Maybe Int
      , maxItems :: Maybe Int
      , uniqueItems :: Maybe Bool
      }
  | StringSchema
      { description :: Maybe Text
      , minLength :: Maybe Int
      , maxLength :: Maybe Int
      , pattern :: Maybe Text
      , enum :: Maybe [Text]
      }
  | NumberSchema
      { description :: Maybe Text
      , minimum :: Maybe Int
      , maximum :: Maybe Int
      , exclusiveMinimum :: Maybe Bool
      , exclusiveMaximum :: Maybe Bool
      , multipleOf :: Maybe Int
      }
  | BooleanSchema
      { description :: Maybe Text
      }
  | NullSchema
      { description :: Maybe Text
      }

newtype ToolCall = ToolCall
  { function :: FunctionCall
  }

data FunctionCall = FunctionCall
  { name :: Text
  , arguments :: JsonValue
  }

data Message = Message
  { role :: Role
  , content :: Text
  , toolCalls :: [ToolCall]
  }

data ChatCompletion = ChatCompletion
  { messages :: [Message]
  , model :: Text
  , tools :: [Tool]
  }

data ChatCompletionResponse = ChatCompletionResponse
  { model :: Text
  , message :: Message
  }

chatCompletion :: ChatCompletion -> IO ChatCompletionResponse
chatCompletion completion = undefined