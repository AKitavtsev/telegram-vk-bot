{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Telegram.Data
  where

import  Data.Aeson
import  Data.Aeson.Types
import  GHC.Generics
import  Data.List
import  Data.Text                    (Text)
import  GHC.Generics

import qualified Data.ByteString.Char8 as BC

import Drop

  
appTelegram :: BC.ByteString
appTelegram = "api.telegram.org"

data Tl_Response a = Tl_Response
  {
    result     :: a
  } deriving (Show, Generic, FromJSON)

  
type UpdatesResponse = Tl_Response [Update]

data Update = Update
  { update_id            :: Int   
  , message              :: Maybe Message 
  , callback_query       :: Maybe CallbackQuery 
  } deriving (FromJSON, Show, Eq, Generic)
  
data Message = Message
  {
    message_id              :: Int 
  , from                    :: Maybe User 
  , text                    :: Maybe Text 
  } deriving (FromJSON, ToJSON, Show, Eq, Generic)
  
data CallbackQuery = CallbackQuery
  {
    cq_id                :: Text
  , cq_from              :: User
  , cq_data              :: Maybe String
  } deriving (Show, Eq, Generic)
  
instance ToJSON CallbackQuery where
  toJSON = toJsonDrop 3
instance FromJSON CallbackQuery where
  parseJSON = parseJsonDrop 3

data User = User
  {
    user_id            :: Int        -- ^ Unique identifier for this user or bot
  , user_is_bot        :: Bool -- ^ True, if this user is a bot
  , user_first_name    :: Text       -- ^ User‘s or bot’s first name
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = parseJsonDrop 5
instance ToJSON User where
  toJSON = toJsonDrop 5
  

data InlineKeyboardMarkup = InlineKeyboardMarkup
  {
    inline_keyboard :: [[InlineKeyboardButton]]
  } deriving (FromJSON, ToJSON, Show, Generic)

data InlineKeyboardButton = InlineKeyboardButton
  {
    ikb_text                             :: Text 
  , ikb_callback_data                    :: Maybe Text 
  } deriving (Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON = toJsonDrop 4
instance FromJSON InlineKeyboardButton where
  parseJSON = parseJsonDrop 4


