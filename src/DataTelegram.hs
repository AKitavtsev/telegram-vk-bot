{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE FlexibleContexts  #-}

module DataTelegram
  where

import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics
import           Data.List
import           Data.Text                    (Text)
import           GHC.Generics

import qualified Data.ByteString.Char8 as BC


-- | Method used to drop prefix from field name during deserialization
parseJsonDrop :: forall a.(Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
parseJsonDrop prefix = genericParseJSON defaultOptions { fieldLabelModifier = drop prefix }

appTelegram :: BC.ByteString
appTelegram = "api.telegram.org"

data Response a = Response
  {
    result     :: a
  } deriving (Show, Generic, FromJSON)
  
type UpdatesResponse = Response [Update]

data Update = Update
  {
    update_id            :: Int   -- ^ The update's unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using 'setWebhooks', since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order.
  , message              :: Maybe Message -- ^ New incoming message of any kind — text, photo, sticker, etc.
  } deriving (FromJSON, Show, Generic)
  
data Message = Message
  {
    message_id              :: Int -- ^ Unique message identifier
  , from                    :: Maybe User -- ^ Sender, can be empty for messages sent to channels
  -- , date                    :: Int -- ^ Date the message was sent in Unix time
  -- , chat                    :: Chat -- ^ Conversation the message belongs to 
  , text                    :: Maybe Text -- ^ Signature of the post author for messages in channels
  } deriving (FromJSON, Show, Generic)
  


data User = User
  {
    user_id            :: Int        -- ^ Unique identifier for this user or bot
  , user_is_bot        :: Bool -- ^ True, if this user is a bot
  , user_first_name    :: Text       -- ^ User‘s or bot’s first name
  } deriving (Show, Generic)

instance FromJSON User where
  parseJSON = parseJsonDrop 5
  
-- data Chat = Chat
  -- { chat_id                             :: Integer
  -- , chat_type                           :: ChatType   -- ^ Type of chat, can be either 'Private', 'Group', 
  -- } deriving (Show, Generic)

-- instance FromJSON Chat
  -- where
    -- parseJSON = parseJsonDrop 5


-- data ChatType = Private
              -- | Group
              -- | Supergroup
              -- | Channel deriving (Show, Generic)

-- instance FromJSON ChatType where
  -- parseJSON "private"    = pure Private
  -- parseJSON "group"      = pure Group
  -- parseJSON "supergroup" = pure Supergroup
  -- parseJSON "channel"    = pure Channel
  -- parseJSON _            = fail "Failed to parse ChatType"
  