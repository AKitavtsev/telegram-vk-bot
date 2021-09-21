{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Types where

import Control.Applicative ((<|>))
import Data.Aeson.Types
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.Generics
import Text.Read (readMaybe)

import qualified Data.ByteString.Char8 as BC

import Bot
import JsonDrop

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

instance Upd Update where
  usId upd = fromMaybe 0 (testMessage <|> testCallbackQuery)
    where
      testMessage =
        case message upd of
          Nothing -> Nothing
          Just (Message _ from _) -> Just $ user_id from
      testCallbackQuery =
        case callback_query upd of
          Nothing -> Nothing
          Just (CallbackQuery _ from _) -> Just $ user_id from
  mesId upd =
    case message upd of
      Nothing -> 0
      Just (Message message_id _ _) -> message_id
  txt upd =
    case message upd of
      Nothing -> ""
      Just (Message _ _ text) -> fromMaybe "" text
  getUserAndNumRep = map fgets
    where
      fgets x = (usId x, fromMaybe 0 (readMaybe (cbData x) :: Maybe Int))
      cbData upd =
        case callback_query upd of
          Nothing -> ""
          Just (CallbackQuery _ _ d) -> fromMaybe "" d
  listUpdWithKey = filter (isJust . callback_query)

data Message =
  Message
    { message_id :: Int
    , from :: User
    , text :: Maybe String
    }
  deriving (FromJSON, ToJSON, Show, Eq, Generic)

data CallbackQuery =
  CallbackQuery
    { cq_id :: String
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
    , user_first_name :: String
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
    { ikb_text :: String
    , ikb_callback_data :: Maybe String
    }
  deriving (Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON = toJsonDrop 4

instance FromJSON InlineKeyboardButton where
  parseJSON = parseJsonDrop 4
