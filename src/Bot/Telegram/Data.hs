{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot.Telegram.Data where
import Data.Aeson.Types
import GHC.Generics

import Data.Text (Text)

import qualified Data.ByteString.Char8 as BC

import Drop

appTelegram :: BC.ByteString
appTelegram = "api.telegram.org"

newtype TlResponse a =
  TlResponse
    { result :: a
    }
  deriving (Show, Generic, FromJSON)

type UpdatesResponse = TlResponse [Update]

data Update =
  Update
    { update_id :: Int
    , message :: Maybe Message
    , callback_query :: Maybe CallbackQuery
    }
  deriving (FromJSON, Show, Eq, Generic)

data Message =
  Message
    { message_id :: Int
    , from :: Maybe User
    , text :: Maybe Text
    }
  deriving (FromJSON, ToJSON, Show, Eq, Generic)

data CallbackQuery =
  CallbackQuery
    { cq_id :: Text
    , cq_from :: User
    , cq_data :: Maybe String
    }
  deriving (Show, Eq, Generic)

instance ToJSON CallbackQuery where
  toJSON = toJsonDrop 3

instance FromJSON CallbackQuery where
  parseJSON = parseJsonDrop 3

data User =
  User
    { user_id :: Int 
    , user_is_bot :: Bool 
    , user_first_name :: Text 
    }
  deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = parseJsonDrop 5

instance ToJSON User where
  toJSON = toJsonDrop 5

newtype InlineKeyboardMarkup =
  InlineKeyboardMarkup
    { inline_keyboard :: [[InlineKeyboardButton]]
    }
  deriving (FromJSON, ToJSON, Show, Generic)

data InlineKeyboardButton =
  InlineKeyboardButton
    { ikb_text :: Text
    , ikb_callback_data :: Maybe Text
    }
  deriving (Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON = toJsonDrop 4

instance FromJSON InlineKeyboardButton where
  parseJSON = parseJsonDrop 4
