{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE FlexibleContexts  #-}

module DataTelegram
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

data Response a = Response
  {
    result     :: a
  } deriving (Show, Generic, FromJSON)

  
type UpdatesResponse = Response [Update]

data Update = Update
  { update_id            :: Int   -- ^ The update's unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using 'setWebhooks', since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order.
  , message              :: Maybe Message -- ^ New incoming message of any kind — text, photo, sticker, etc.
  , callback_query       :: Maybe CallbackQuery -- ^ This object represents an incoming callback query from a callback button in an inline keyboard. If the button that originated the query was attached to a message sent by the bot, the field message will be presented. If the button was attached to a message sent via the bot (in inline mode), the field inline_message_id will be presented.
  } deriving (FromJSON, Show, Generic)
  
data Message = Message
  {
    message_id              :: Int -- ^ Unique message identifier
  , from                    :: Maybe User -- ^ Sender, can be empty for messages sent to channels
  -- , date                    :: Int -- ^ Date the message was sent in Unix time
  -- , chat                    :: Chat -- ^ Conversation the message belongs to 
  , text                    :: Maybe Text -- ^ Signature of the post author for messages in channels
  } deriving (FromJSON, ToJSON, Show, Eq, Generic)
  
data CallbackQuery = CallbackQuery
  {
    cq_id                :: Text
  , cq_from              :: User
  -- , cq_message           :: Maybe Message
  -- , cq_inline_message_id :: Maybe Text
  -- , cq_chat_instance     :: Text
  -- , cq_data              :: Maybe Text
     , cq_data              :: Maybe String
  -- , cq_game_short_name   :: Maybe Text
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
    ikb_text                             :: Text -- ^ Label text on the button
  -- , ikb_url                              :: Maybe Text -- ^ HTTP url to be opened when button is pressed
  , ikb_callback_data                    :: Maybe Text -- ^ Data to be sent in a callback query to the bot when button is pressed, 1-64 bytes
  -- , ikb_switch_inline_query              :: Maybe Text -- ^  If set, pressing the button will prompt the user to select one of their chats, open that chat and insert the bot‘s username and the specified inline query in the input field. Can be empty, in which case just the bot’s username will be inserted.
  -- , ikb_callback_game                    :: Maybe CallbackGame -- ^  Description of the game that will be launched when the user presses the button. NOTE: This type of button must always be the first button in the first row.
  -- , ikb_switch_inline_query_current_chat :: Maybe Text -- ^ If set, pressing the button will insert the bot‘s username and the specified inline query in the current chat's input field. Can be empty, in which case only the bot’s username will be inserted.
  -- , ikb_pay                              :: Maybe Bool -- ^ Specify True, to send a Pay button. NOTE: This type of button must always be the first button in the first row.
  } deriving (Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON = toJsonDrop 4
instance FromJSON InlineKeyboardButton where
  parseJSON = parseJsonDrop 4


