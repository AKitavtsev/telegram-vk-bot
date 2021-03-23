{-# LANGUAGE TemplateHaskell #-}

module Bot
    ( Handle (..)
    , UPD (..)
    , loopBot
    )
        where

import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Simple
import Control.Monad.State

import Drop
import Config
import MapR
import Telegram.Data
import VK.Data

import qualified Log

data Handle = Handle
    { config               :: Config
    , handlerLog           :: Log.Handle
    , session              :: Session
    , dictionary           :: MapInt
    , offset               :: String
    , updates              :: [UPD]
    , initSession          :: Bot.Handle -> IO ()
    , getUpdates           :: Bot.Handle -> IO Bot.Handle
    , copyMessages         :: Bot.Handle -> IO Bot.Handle
    , sendMessagesWithKb   :: Bot.Handle -> IO Bot.Handle
    , sendMessagesWithHelp :: Bot.Handle -> IO Bot.Handle
    , newDict              :: Bot.Handle -> IO Bot.Handle
    }


data UPD = Tl Update| VK Event  deriving (Show, Eq)
    
loopBot :: Bot.Handle -> IO Bot.Handle
loopBot handle = return handle               >>=
                 getUpdates handle           >>=
                 copyMessages handle         >>=
                 sendMessagesWithKb handle   >>= 
                 sendMessagesWithHelp handle >>=
                 newDict handle              >>=
                 loopBot
                

